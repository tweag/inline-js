{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Internals.IPC
  ( IPCOpts(..)
  , defIPCOpts
  , IPCSession(..)
  , newIPCSession
  , killIPCSession
  , ipcSend
  , ipcRecvAll
  ) where

import Control.DeepSeq
import Control.Exception
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
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
  pure
    IPCSession
      { nodeProc = _ph
      , nodeStdIn = _stdin
      , nodeStdOut = _stdout
      , nodeStdErr = _m_stderr
      }

killIPCSession :: IPCSession -> IO ()
killIPCSession IPCSession {..} = terminateProcess nodeProc

ipcSend :: IPCSession -> LBS.ByteString -> IO ()
ipcSend IPCSession {..} buf = do
  hPutBuilder nodeStdIn $
    word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
  hFlush nodeStdIn

ipcRecvAll :: IPCSession -> IO LBS.ByteString
ipcRecvAll IPCSession {..} = do
  buf <- LBS.hGetContents nodeStdOut
  evaluate $ force buf
