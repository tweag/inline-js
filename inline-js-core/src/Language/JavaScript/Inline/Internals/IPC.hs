{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Internals.IPC
  ( IPCOpts(..)
  , defIPCOpts
  , IPCSession
  , newIPCSession
  , killIPCSession
  , ipcSend
  , ipcRecv
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import qualified Paths_inline_js_core
import System.FilePath
import System.IO
import System.Process

data IPCOpts = IPCOpts
  { nodePath :: FilePath
  , nodeStdErrCopyBack :: Bool
  }

defIPCOpts :: IPCOpts
defIPCOpts = IPCOpts {nodePath = "node", nodeStdErrCopyBack = False}

data IPCSession = IPCSession
  { nodeProc :: ProcessHandle
  , nodeStdIn, nodeStdOut :: Handle
  , nodeStdErr :: Maybe Handle
  , nodeStdInLock, nodeStdOutLock :: MVar ()
  }

newIPCSession :: IPCOpts -> IO IPCSession
newIPCSession IPCOpts {..} = do
  _datadir <- Paths_inline_js_core.getDataDir
  (Just _stdin, Just _stdout, _m_stderr, _ph) <-
    createProcess
      (proc
         nodePath
         ["--experimental-modules", _datadir </> "jsbits" </> "main.mjs"])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err =
            if nodeStdErrCopyBack
              then Inherit
              else CreatePipe
        }
  hSetBinaryMode _stdin True
  hSetBuffering _stdin $ BlockBuffering Nothing
  hSetBinaryMode _stdout True
  hSetBuffering _stdout $ BlockBuffering Nothing
  _stdin_lock <- newMVar ()
  _stdout_lock <- newMVar ()
  pure
    IPCSession
      { nodeProc = _ph
      , nodeStdIn = _stdin
      , nodeStdOut = _stdout
      , nodeStdErr = _m_stderr
      , nodeStdInLock = _stdin_lock
      , nodeStdOutLock = _stdout_lock
      }

killIPCSession :: IPCSession -> IO ()
killIPCSession IPCSession {..} = terminateProcess nodeProc

ipcSend :: IPCSession -> LBS.ByteString -> IO ()
ipcSend IPCSession {..} buf =
  withMVar nodeStdInLock $ \_ -> do
    hPutBuilder nodeStdIn $
      word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
    hFlush nodeStdIn

ipcRecv :: IPCSession -> IO LBS.ByteString
ipcRecv IPCSession {..} =
  withMVar nodeStdOutLock $ \_ -> do
    lbuf <- BS.hGet nodeStdOut 4
    len <-
      BS.unsafeUseAsCStringLen lbuf $ \(lptr, _) ->
        fromIntegral <$> peek (castPtr lptr :: Ptr Word32)
    buf <- LBS.hGet nodeStdOut len
    evaluate $ force buf
