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
import Data.Foldable
import Data.Functor
import Data.Word
import Foreign.Ptr
import Foreign.StablePtr
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Core.Internal
import Language.JavaScript.Inline.Core.Message.Class
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
        -- | Available when the corresponding inherit flag in 'JSSessionOpts' is
        -- 'False'.
        nodeStdIn, nodeStdOut, nodeStdErr :: Maybe Handle
      }

-- | Initializes a new 'JSSession'.
newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  checkNodeVersion nodePath
  (jsbits_dir, jss) <- do
    jsbits_dir <- (</> "jsbits") <$> Paths_inline_js_core.getDataDir
    case nodeWorkDir of
      Just p -> do
        jss <- listDirectory jsbits_dir
        for_ jss $ \js -> copyFile (jsbits_dir </> js) (p </> js)
        pure (p, jss)
      _ -> pure (jsbits_dir, [])
  parent_env <- getEnvironment
  (host_read_h, node_write_h) <- createPipe
  (node_read_h, host_write_h) <- createPipe
  node_write_fd <- handleToFd node_write_h
  node_read_fd <- handleToFd node_read_h
  (_m_stdin, _m_stdout, _m_stderr, _ph) <-
    createProcess
      ( proc nodePath $
          nodeExtraArgs
            <> [ "--experimental-modules",
                 jsbits_dir </> "eval.js",
                 show node_write_fd,
                 show node_read_fd
               ]
      )
        { cwd = nodeWorkDir,
          env = Just $ nodeExtraEnv <> parent_env,
          std_in = if nodeStdInInherit then Inherit else CreatePipe,
          std_out = if nodeStdOutInherit then Inherit else CreatePipe,
          std_err = if nodeStdErrInherit then Inherit else CreatePipe
        }
  hClose node_write_h
  hClose node_read_h
  send_queue <- newTQueueIO
  _ <-
    forkIO $
      let w = do
            buf <- atomically $ readTQueue send_queue
            hPutBuilder host_write_h $
              word32LE (fromIntegral $ LBS.length buf)
                <> lazyByteString buf
            hFlush host_write_h
            w
       in void $ tryAny w
  _ <-
    forkIO $
      let w = do
            (len :: Word32) <- peekHandle host_read_h
            (msg_id :: Word32) <- peekHandle host_read_h
            let len' = fromIntegral len - 4
                sp = castPtrToStablePtr $ intPtrToPtr $ fromIntegral msg_id
            mv <- deRefStablePtr sp
            freeStablePtr sp
            buf <- hGetLBS host_read_h len'
            putMVar mv buf
            w
       in void $ tryAny w
  _close <- once $ do
    atomically $ writeTQueue send_queue "SHUTDOWN"
    case nodeWorkDir of
      Just p -> for_ jss $ \js -> removeFile $ p </> js
      _ -> pure ()
  pure JSSession
    { closeJSSession = _close,
      sendData = atomically . writeTQueue send_queue,
      nodeStdIn = _m_stdin,
      nodeStdOut = _m_stdout,
      nodeStdErr = _m_stderr
    }

-- | Use 'bracket' to ensure 'closeJSSession' is called.
withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (newJSSession opts) closeJSSession

sendMsg :: (Request req, Response resp) => JSSession -> req -> IO (IO resp)
sendMsg JSSession {..} msg = do
  mv <- newEmptyMVar
  sp <- newStablePtr mv
  sendData $ encodeRequest sp msg
  once $
    takeMVar mv >>= decodeResponse
