{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline.Core.Session where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (bracket)
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Distribution.Simple.Utils
import Foreign
import Language.JavaScript.Inline.Core.Bracket
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.IPC
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.NodePath
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
    -- | Size in MiBs of the buffer for passing results of functions exported by
    -- 'exportSync'. Most users don't need to care about this. Defaults to 1.
    nodeExportSyncBufferSize :: Int
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { nodePath = defNodePath,
      nodeExtraArgs =
        [ "--experimental-modules",
          "--experimental-worker",
          "--no-warnings",
          "--unhandled-rejections=strict"
        ],
      nodeExtraEnv = [],
      nodeModules = Nothing,
      nodeExportSyncBufferSize = 1
    }

data Session = Session
  { ipc :: IPC,
    fatalErrorInbox :: TMVar (Either LBS.ByteString LBS.ByteString),
    -- | After a 'Session' is closed, no more messages can be sent to @node@.
    -- Use this to close the 'Session' if @node@ should still run for some time
    -- to allow previous evaluation results to be sent back.
    closeSession :: IO (),
    -- | Terminate the @node@ process immediately. Use this to close the
    -- 'Session' if @node@ doesn't need to run any more.
    killSession :: IO ()
  }

instance Show Session where
  show Session {} = "Session"

newSession :: Config -> IO Session
newSession Config {..} = do
  unless rtsSupportsBoundThreads $ throwIO NotThreadedRTS
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
                [ ( "INLINE_JS_EXPORT_SYNC_BUFFER_SIZE",
                    show nodeExportSyncBufferSize
                  )
                ]
                  <> map ("INLINE_JS_NODE_MODULES",) (maybeToList nodeModules)
                  <> nodeExtraEnv
                  <> _env,
          std_in = CreatePipe,
          std_out = CreatePipe
        }
  _inbox <- newEmptyTMVarIO
  mdo
    let on_recv msg_buf = do
          msg <- runGetExact messageJSGet msg_buf
          case msg of
            JSEvalResponse {..} -> do
              let _sp = word64ToStablePtr jsEvalResponseId
              _inbox <- deRefStablePtr _sp
              atomically $ putTMVar _inbox jsEvalResponseContent
            HSEvalRequest {..} -> do
              _ <-
                forkFinally
                  ( do
                      let sp = word64ToStablePtr hsEvalRequestFunc
                      f <- deRefStablePtr sp
                      r <- f args
                      sessionSend
                        _session
                        HSEvalResponse
                          { hsEvalResponseIsSync = hsEvalRequestIsSync,
                            hsEvalResponseId = hsEvalRequestId,
                            hsEvalResponseContent = Right r
                          }
                  )
                  ( \case
                      Left (SomeException err) -> do
                        let err_buf = stringToLBS $ show err
                        sessionSend
                          _session
                          HSEvalResponse
                            { hsEvalResponseIsSync = hsEvalRequestIsSync,
                              hsEvalResponseId = hsEvalRequestId,
                              hsEvalResponseContent = Left err_buf
                            }
                      Right () -> pure ()
                  )
              pure ()
            -- todo: should make all subsequent operations invalid immediately
            -- here, possibly via a session state atomic variable. also cleanup
            -- tmp dir.
            FatalError err_buf -> atomically $ putTMVar _inbox $ Left err_buf
        ipc_post_close = do
          _ <- waitForProcess _ph
          removePathForcibly _root
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
        session_kill = do
          terminateProcess _ph
          ipc_post_close
        _session =
          Session
            { ipc = _ipc,
              fatalErrorInbox = _inbox,
              closeSession = session_close,
              killSession = session_kill
            }
    pure _session

withSession :: Config -> (Session -> IO r) -> IO r
withSession c = bracket (newSession c) closeSession

sessionSend :: Session -> MessageHS -> IO ()
sessionSend Session {..} msg = send ipc $ toLazyByteString $ messageHSPut msg
