{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Core.Session
  ( JSSessionOpts(..)
  , defJSSessionOpts
  , JSSession
  , newJSSession
  , closeJSSession
  , withJSSession
  , sendMsg
  , recvMsg
  , sendRecv
  , nodeStdIn
  , nodeStdOut
  , nodeStdErr
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.Word
import Foreign
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.MessageCounter
import qualified Paths_inline_js_core
import System.Directory
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process

data JSSessionOpts = JSSessionOpts
  { nodePath :: FilePath
  , nodeExtraArgs :: [String]
  , nodeWorkDir :: Maybe FilePath
  , nodeStdInInherit, nodeStdOutInherit, nodeStdErrInherit :: Bool
  }

{-# NOINLINE defJSSessionOpts #-}
defJSSessionOpts :: JSSessionOpts
defJSSessionOpts =
  unsafePerformIO $ do
    _datadir <- Paths_inline_js_core.getDataDir
    pure
      JSSessionOpts
        { nodePath = "node"
        , nodeExtraArgs = []
        , nodeWorkDir = Nothing
        , nodeStdInInherit = False
        , nodeStdOutInherit = False
        , nodeStdErrInherit = False
        }

data JSSession = JSSession
  { closeJSSession :: IO ()
  , sendData :: LBS.ByteString -> IO ()
  , recvData :: MsgId -> IO LBS.ByteString
  , nodeStdIn, nodeStdOut, nodeStdErr :: Maybe Handle
  , msgCounter :: MsgCounter
  }

hGet' :: Handle -> Ptr a -> Int -> IO ()
hGet' h p l = do
  l' <- hGetBuf h p l
  unless (l' == l) $
    fail $ "hGet': expected " <> show l <> " bytes, got " <> show l'

newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  (mjss_dir, mjss) <-
    do mjss_dir <- (</> "jsbits") <$> Paths_inline_js_core.getDataDir
       case nodeWorkDir of
         Just p -> do
           mjss <- listDirectory mjss_dir
           for_ mjss $ \mjs -> copyFile (mjss_dir </> mjs) (p </> mjs)
           pure (p, mjss)
         _ -> pure (mjss_dir, [])
  (rh0, wh0) <- createPipe
  (rh1, wh1) <- createPipe
  for_ [rh0, wh0, rh1, wh1] $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
  wfd0 <- handleToFd wh0
  rfd1 <- handleToFd rh1
  (_m_stdin, _m_stdout, _m_stderr, _ph) <-
    createProcess
      (proc nodePath $
       nodeExtraArgs <>
       ["--experimental-modules", mjss_dir </> "eval.mjs", show wfd0, show rfd1])
        { cwd = nodeWorkDir
        , std_in =
            if nodeStdInInherit
              then Inherit
              else CreatePipe
        , std_out =
            if nodeStdOutInherit
              then Inherit
              else CreatePipe
        , std_err =
            if nodeStdErrInherit
              then Inherit
              else CreatePipe
        }
  send_queue <- newTQueueIO
  send_tid <-
    forkIO $
    let w = do
          buf <- atomically $ readTQueue send_queue
          hPutBuilder wh1 $
            word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
          w
     in w
  recv_map <- newTVarIO IntMap.empty
  recv_tid <-
    forkIO $
    let w = do
          len <-
            alloca $ \p -> do
              hGet' rh0 p 4
              peek p
          msg_id <-
            alloca $ \p -> do
              hGet' rh0 p 4
              peek p
          let len' = fromIntegral (len :: Word32) - 4
              msg_id' = fromIntegral (msg_id :: Word32)
          buf <- fmap LBS.fromStrict $ BS.create len' $ \p -> hGet' rh0 p len'
          atomically $ modifyTVar' recv_map $ IntMap.insert msg_id' buf
          w
     in w
  _msg_counter <- newMsgCounter
  pure
    JSSession
      { closeJSSession =
          do killThread send_tid
             killThread recv_tid
             terminateProcess _ph
             case nodeWorkDir of
               Just p -> for_ mjss $ \mjs -> removeFile $ p </> mjs
               _ -> pure ()
      , sendData =
          \buf -> do
            buf' <- evaluate $ force buf
            atomically $ writeTQueue send_queue buf'
      , recvData =
          \msg_id ->
            atomically $ do
              m <- readTVar recv_map
              case IntMap.updateLookupWithKey
                     (\_ _ -> Nothing)
                     (coerce msg_id)
                     m of
                (Just buf, m') -> do
                  writeTVar recv_map m'
                  pure buf
                _ -> retry
      , nodeStdIn = _m_stdin
      , nodeStdOut = _m_stdout
      , nodeStdErr = _m_stderr
      , msgCounter = _msg_counter
      }

withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (newJSSession opts) closeJSSession

sendMsg :: Request r => JSSession -> r -> IO MsgId
sendMsg JSSession {..} msg = do
  msg_id <- newMsgId msgCounter
  sendData $ encodeRequest msg_id msg
  pure msg_id

recvMsg :: Response r => JSSession -> MsgId -> IO r
recvMsg JSSession {..} msg_id = do
  buf <- recvData msg_id
  decodeResponse buf

sendRecv :: (Request req, Response resp) => JSSession -> req -> IO resp
sendRecv s = recvMsg s <=< sendMsg s
