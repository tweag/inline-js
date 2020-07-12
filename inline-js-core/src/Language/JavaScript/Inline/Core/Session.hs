{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline.Core.Session where

import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.Foldable
import Data.IORef
import qualified Data.IntSet as IS
import Data.Maybe
import Distribution.Simple.Utils
import Foreign
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.IPC
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.NodeVersion
import Language.JavaScript.Inline.Core.Utils
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.Process

-- $session-todos
--
-- * Using closed sessions throw immediately
-- * Handle errors in send/recv thread

{-# NOINLINE evalServerSrc #-}
evalServerSrc :: BS.ByteString
evalServerSrc = $(embedFile $ "jsbits" </> "index.js")

data Config = Config
  { -- | Path to the @node@ executable. Defaults to @node@.
    nodePath :: FilePath,
    -- | Extra @node@ arguments that appear before the eval server script's file
    -- path. These arguments won't show up in @process.argv@.
    nodeExtraArgs :: [String],
    -- | Extra environment variables to pass to @node@. Will shadow already
    -- existing ones.
    nodeExtraEnv :: [(String, String)],
    -- | To @require()@ or @import()@ third-party packages, set this to the
    -- @node_modules@ directory path.
    nodeModules :: Maybe FilePath,
    -- | By default, an 'EvalError' only throws for a single return value and
    -- doesn't affect later evaluation. Set this to 'True' if the @node@ process
    -- should terminate immediately upon an 'EvalError'.
    nodeExitOnEvalError :: Bool
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { nodePath = "node",
      nodeExtraArgs =
        [ "--experimental-modules",
          "--experimental-worker",
          "--no-warnings",
          "--unhandled-rejections=strict"
        ],
      nodeExtraEnv = [],
      nodeModules = Nothing,
      nodeExitOnEvalError = False
    }

data Session = Session
  { ipc :: IPC,
    pendingCallbacks :: IORef IS.IntSet,
    -- | After a 'Session' is closed, no more messages can be sent to @node@.
    -- @node@ may still run for some time to allow previous evaluation results
    -- to be sent back.
    closeSession :: IO ()
  }

instance Show Session where
  show Session {} = "Session"

newSession :: Config -> IO Session
newSession Config {..} = do
  checkNodeVersion nodePath
  (_root, _p) <- do
    _tmp <- getTemporaryDirectory
    _root <- createTempDirectory _tmp "inline-js"
    let _p = _root </> "index.js"
    BS.writeFile _p evalServerSrc
    pure (_root, _p)
  _env <- getEnvironment
  (Just _wh, Just _rh, _, _ph) <-
    createProcess
      (proc nodePath $ nodeExtraArgs <> [_p])
        { env =
            Just $
              kvDedup $
                [("INLINE_JS_EXIT_ON_EVAL_ERROR", "1") | nodeExitOnEvalError]
                  <> map ("INLINE_JS_NODE_MODULES",) (maybeToList nodeModules)
                  <> nodeExtraEnv
                  <> _env,
          std_in = CreatePipe,
          std_out = CreatePipe
        }
  _cbs_ref <- newIORef IS.empty
  let on_recv msg_buf = do
        msg <- runGetExact messageJSGet msg_buf
        case msg of
          JSEvalResponse {..} -> do
            let sp = word64ToStablePtr responseId
            atomicModifyIORef' _cbs_ref $
              \_cbs -> (IS.delete (intFromStablePtr sp) _cbs, ())
            cb <- deRefStablePtr sp
            freeStablePtr sp
            cb responseContent
          -- todo: should make all subsequent operations invalid immediately
          -- here, possibly via a session state atomic variable. also cleanup
          -- tmp dir.
          FatalError err_buf -> do
            _cbs <- atomicModifyIORef _cbs_ref (throw SessionClosed,)
            for_ (IS.toList _cbs) $ \_cb_id -> do
              let sp = intToStablePtr _cb_id
              cb <- deRefStablePtr sp
              freeStablePtr sp
              cb $ Left err_buf
      ipc_post_close = do
        _ <- waitForProcess _ph
        pure ()
  _ipc <-
    ipcFork $
      ipcFromHandles
        _wh
        _rh
        IPC
          { send = error "newSession: send",
            recv = error "newSession: recv",
            onRecv = on_recv,
            closeMsg = toLazyByteString $ messageHSPut Close,
            preClose = error "newSession: preClose",
            postClose = ipc_post_close
          }
  let session_close = do
        send _ipc $ closeMsg _ipc
        ipc_post_close
        removeDirectoryRecursive _root
  pure
    Session
      { ipc = _ipc,
        pendingCallbacks = _cbs_ref,
        closeSession = session_close
      }

sessionSend :: Session -> MessageHS -> IO ()
sessionSend Session {..} msg = send ipc $ toLazyByteString $ messageHSPut msg
