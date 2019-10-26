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
    newHSFunc,
    nodeStdIn,
    nodeStdOut,
    nodeStdErr,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Binary.Put
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Foldable
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Word
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Core.HSCode
import Language.JavaScript.Inline.Core.Internal
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.Message.HSCode
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
        nodeStdInInherit, nodeStdOutInherit, nodeStdErrInherit :: Bool,
        -- | Shared memory size of @node@ in megabytes.
        nodeSharedMemSize :: Int
      }

-- | A sensible default 'JSSessionOpts'.
--
-- Uses @node@ from @PATH@ and sets the inherit flags to 'False'. Shared memory
-- size defaults to 1MB.
--
-- See doc of 'Language.JavaScript.Inline.Core.exportSyncHSFunc' for what the
-- "shared memory" is about.
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
      nodeStdErrInherit = False,
      nodeSharedMemSize = 1
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
        msgCounter :: MsgCounter,
        hsFuncs :: IORef (IntMap HSFunc, Int)
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
                 show nodeSharedMemSize,
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
  _hs_funcs <- newIORef (IntMap.empty, 1)
  recv_map <- newTVarIO IntMap.empty
  recv_tid <-
    forkIO $
      let w = do
            (len :: Word32) <- peekHandle rh0
            (msg_id :: Word32) <- peekHandle rh0
            if odd msg_id
              then do
                let len' = fromIntegral len - 4
                    msg_id' = fromIntegral msg_id
                buf <- hGetLBS rh0 len'
                atomically $ modifyTVar' recv_map $ IntMap.insert msg_id' buf
              else do
                (_ :: Word32) <- peekHandle rh0
                (hs_func_ref :: Word32) <- peekHandle rh0
                (args_len :: Word32) <- peekHandle rh0
                args <- replicateM (fromIntegral args_len) $ do
                  (arg_len :: Word32) <- peekHandle rh0
                  hGetLBS rh0 (fromIntegral arg_len)
                void $ forkIO $ do
                  r <- tryAny $ do
                    let ref = fromIntegral hs_func_ref
                    hs_func <- (IntMap.! ref) . fst <$> readIORef _hs_funcs
                    runHSFunc hs_func args
                  atomically $ writeTQueue send_queue $ runPut $ do
                    putWord32host msg_id
                    putWord32host 4
                    case r of
                      Left err -> do
                        putWord32host 1
                        putBuilder $ stringUtf8 $ show err
                      Right buf -> do
                        putWord32host 0
                        putLazyByteString buf
            w
       in w
  _msg_counter <- newMsgCounter
  _close <- once $ do
    killThread send_tid
    killThread recv_tid
    terminateProcess _ph
    atomicWriteIORef _hs_funcs (IntMap.empty, 1)
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
      msgCounter = _msg_counter,
      hsFuncs = _hs_funcs
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

newHSFunc :: JSSession -> Bool -> HSFunc -> IO (ExportHSFuncRequest, IO ())
newHSFunc JSSession {..} s f = atomicModifyIORef' hsFuncs $ \(m, l) ->
  ( (IntMap.insert l f m, succ l),
    ( ExportHSFuncRequest {sync = s, exportHSFuncRef = coerce l},
      atomicModifyIORef' hsFuncs $ \(m', l') -> ((IntMap.delete l m', l'), ())
    )
  )
