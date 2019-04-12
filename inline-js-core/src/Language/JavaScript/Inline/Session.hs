{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Session
  ( JSSessionOpts(..)
  , defJSSessionOpts
  , setJSSessionDebug
  , setJSSessionWorkDir
  , JSSession
  , newJSSession
  , closeJSSession
  , withJSSession
  , sendMsg
  , recvMsg
  , sendRecv
  , withNodeStdIn
  , withNodeStdOut
  , withNodeStdErr
  ) where

import Control.Exception
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Language.JavaScript.Inline.Message.Class
import Language.JavaScript.Inline.MessageCounter
import Language.JavaScript.Inline.Transport.Process
import Language.JavaScript.Inline.Transport.Type
import Language.JavaScript.Inline.Transport.Utils
import qualified Paths_inline_js_core
import System.IO
import System.IO.Unsafe

newtype JSSessionOpts = JSSessionOpts
  { nodeProcessTransportOpts :: ProcessTransportOpts
  }

{-# NOINLINE defJSSessionOpts #-}
defJSSessionOpts :: JSSessionOpts
defJSSessionOpts =
  unsafePerformIO $ do
    _datadir <- Paths_inline_js_core.getDataDir
    pure
      JSSessionOpts
        { nodeProcessTransportOpts =
            ProcessTransportOpts
              { nodePath = "node"
              , nodeExtraArgs = []
              , nodeWorkDir = Nothing
              , nodeStdInInherit = False
              , nodeStdOutInherit = False
              , nodeStdErrInherit = False
              }
        }

setJSSessionDebug :: JSSessionOpts -> JSSessionOpts
setJSSessionDebug opts =
  opts
    { nodeProcessTransportOpts =
        (nodeProcessTransportOpts opts) {nodeStdErrInherit = True}
    }

setJSSessionWorkDir :: FilePath -> JSSessionOpts -> JSSessionOpts
setJSSessionWorkDir p opts =
  opts
    { nodeProcessTransportOpts =
        (nodeProcessTransportOpts opts) {nodeWorkDir = Just p}
    }

data JSSession = JSSession
  { nodeTransport :: Transport
  , nodeStdIn, nodeStdOut, nodeStdErr :: Maybe Handle
  , msgCounter :: MsgCounter
  , msgRecv :: MsgId -> IO LBS.ByteString
  }

newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  (t0, _m_stdin, _m_stdout, _m_stderr) <-
    newProcessTransport nodeProcessTransportOpts
  t1 <- lockSend $ strictTransport t0
  (_uniq_recv, t2) <- uniqueRecv (runGet (fromIntegral <$> getWord32host)) t1
  _msg_counter <- newMsgCounter
  pure
    JSSession
      { nodeTransport = t2
      , nodeStdIn = _m_stdin
      , nodeStdOut = _m_stdout
      , nodeStdErr = _m_stderr
      , msgCounter = _msg_counter
      , msgRecv = coerce _uniq_recv
      }

closeJSSession :: JSSession -> IO ()
closeJSSession JSSession {..} = closeTransport nodeTransport

withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (newJSSession opts) closeJSSession

sendMsg :: Request r => JSSession -> r -> IO MsgId
sendMsg JSSession {..} msg = do
  msg_id <- newMsgId msgCounter
  sendData nodeTransport $ encodeRequest msg_id msg
  pure msg_id

recvMsg :: Response r => JSSession -> MsgId -> IO r
recvMsg JSSession {..} msg_id = do
  buf <- msgRecv msg_id
  decodeResponse msg_id buf

sendRecv :: (Request req, Response resp) => JSSession -> req -> IO resp
sendRecv s = recvMsg s <=< sendMsg s

withNodeStdIn, withNodeStdOut, withNodeStdErr ::
     JSSession -> (Handle -> IO r) -> IO r
withNodeStdIn JSSession {..} c =
  case nodeStdIn of
    Just h -> c h
    _ -> fail "stdin handle not available"

withNodeStdOut JSSession {..} c =
  case nodeStdOut of
    Just h -> c h
    _ -> fail "stdout handle not available"

withNodeStdErr JSSession {..} c =
  case nodeStdErr of
    Just h -> c h
    _ -> fail "stderr handle not available"
