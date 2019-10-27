{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Core.Session
  ( JSSessionOpts (..),
    defJSSessionOpts,
    JSSession,
    newJSSession,
    closeJSSession,
    withJSSession,
    sendMsg,
    nodeStdIn,
    nodeStdOut,
    nodeStdErr,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.Word
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Core.Internal
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.MessageCounter
import Language.JavaScript.Inline.Core.NodeVersion
import qualified Paths_inline_js_core
import System.Directory
import System.Environment.Blank
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

-- | Options to initialize a 'JSSession'.
data JSSessionOpts
  = JSSessionOpts
      { -- | Path to the @node@ executable.
        nodePath :: FilePath,
        -- | Extra arguments passed to @node@. Can't be accessed via
        -- @process.argv@.
        nodeExtraArgs :: [String],
        -- | Working directory of @node@. To @import()@ npm installed modules,
        -- set to the directory containing @node_modules@.
        nodeWorkDir :: Maybe FilePath,
        -- | Extra environment variables to be passed to @node@.
        nodeExtraEnv :: [(String, String)],
        -- | Inherit stdio handles of @node@ from the host process.
        nodeStdInInherit, nodeStdOutInherit, nodeStdErrInherit :: Bool
      }

-- | A sensible default 'JSSessionOpts'.
--
-- Uses @node@ from @PATH@ and sets the inherit flags to 'False'.
{-# NOINLINE defJSSessionOpts #-}
defJSSessionOpts :: JSSessionOpts
defJSSessionOpts = unsafePerformIO $ do
  _datadir <- Paths_inline_js_core.getDataDir
  pure JSSessionOpts
    { nodePath = "node",
      nodeExtraArgs = [],
      nodeWorkDir = Nothing,
      nodeExtraEnv = [],
      nodeStdInInherit = False,
      nodeStdOutInherit = False,
      nodeStdErrInherit = False
    }

-- | Represents an active @node@ process and related IPC states.
data JSSession
  = JSSession
      { -- | Shuts down a 'JSSession'. Safe to call more than once.
        closeJSSession :: IO (),
        sendData :: LBS.ByteString -> IO (),
        recvData :: MsgId -> IO LBS.ByteString,
        -- | Available when the corresponding inherit flag in 'JSSessionOpts' is
        -- 'False'.
        nodeStdIn, nodeStdOut, nodeStdErr :: Maybe Handle,
        msgCounter :: MsgCounter
      }

-- | Initializes a new 'JSSession'.
newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  checkNodeVersion nodePath
  (mjss_dir, mjss) <- do
    mjss_dir <- (</> "jsbits") <$> Paths_inline_js_core.getDataDir
    case nodeWorkDir of
      Just p -> do
        mjss <- listDirectory mjss_dir
        for_ mjss $ \mjs -> copyFile (mjss_dir </> mjs) (p </> mjs)
        pure (p, mjss)
      _ -> pure (mjss_dir, [])
  parent_env <- getEnvironment
  (rh0, wh0) <- createPipe
  (rh1, wh1) <- createPipe
  for_ [rh0, wh0, rh1, wh1] $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
  wfd0 <- handleToFd wh0
  rfd1 <- handleToFd rh1
  (_m_stdin, _m_stdout, _m_stderr, _ph) <-
    createProcess
      ( proc nodePath $
          nodeExtraArgs
            <> [ "--experimental-modules",
                 mjss_dir </> "eval.mjs",
                 show wfd0,
                 show rfd1
               ]
      )
        { cwd = nodeWorkDir,
          env = Just $ nodeExtraEnv <> parent_env,
          std_in = if nodeStdInInherit then Inherit else CreatePipe,
          std_out = if nodeStdOutInherit then Inherit else CreatePipe,
          std_err = if nodeStdErrInherit then Inherit else CreatePipe
        }
  send_queue <- newTQueueIO
  send_tid <-
    forkIO $
      let w = do
            buf <- atomically $ readTQueue send_queue
            hPutBuilder wh1 $
              word32LE (fromIntegral $ LBS.length buf)
                <> lazyByteString buf
            w
       in w
  recv_map <- newTVarIO IntMap.empty
  recv_tid <-
    forkIO $
      let w = do
            (len :: Word32) <- peekHandle rh0
            (msg_id :: Word32) <- peekHandle rh0
            let len' = fromIntegral len - 4
                msg_id' = fromIntegral msg_id
            buf <- hGetLBS rh0 len'
            atomically $ modifyTVar' recv_map $ IntMap.insert msg_id' buf
            w
       in w
  _msg_counter <- newMsgCounter
  _close <- once $ do
    killThread send_tid
    killThread recv_tid
    terminateProcess _ph
    case nodeWorkDir of
      Just p -> for_ mjss $ \mjs -> removeFile $ p </> mjs
      _ -> pure ()
  pure JSSession
    { closeJSSession = _close,
      sendData = atomically . writeTQueue send_queue,
      recvData = \msg_id -> atomically $ do
        m <- readTVar recv_map
        case IntMap.updateLookupWithKey
          (\_ _ -> Nothing)
          (coerce msg_id)
          m of
          (Just buf, m') -> do
            writeTVar recv_map m'
            pure buf
          _ -> retry,
      nodeStdIn = _m_stdin,
      nodeStdOut = _m_stdout,
      nodeStdErr = _m_stderr,
      msgCounter = _msg_counter
    }

-- | Use 'bracket' to ensure 'closeJSSession' is called.
withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (newJSSession opts) closeJSSession

sendMsg ::
  forall req resp.
  (Request req, Response resp) =>
  JSSession ->
  req ->
  IO (IO resp)
sendMsg JSSession {..} msg = do
  msg_id <- newMsgId msgCounter
  sendData $ encodeRequest msg_id msg
  once $ do
    buf <- recvData msg_id
    decodeResponse buf
