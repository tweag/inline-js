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
import Language.JavaScript.Inline.Message.Class
import Language.JavaScript.Inline.MessageCounter
import Language.JavaScript.Inline.Transport.Process
import Language.JavaScript.Inline.Transport.Type
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
  }

newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  (t, _m_stdin, _m_stdout, _m_stderr) <-
    newProcessTransport nodeProcessTransportOpts
  _msg_counter <- newMsgCounter
  pure
    JSSession
      { nodeTransport = t
      , nodeStdIn = _m_stdin
      , nodeStdOut = _m_stdout
      , nodeStdErr = _m_stderr
      , msgCounter = _msg_counter
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
  buf <- recvData nodeTransport msg_id
  decodeResponse buf

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
