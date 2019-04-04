{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Session
  ( JSSessionOpts(..)
  , defJSSessionOpts
  , JSSession
  , newJSSession
  , closeJSSession
  , withJSSession
  , sendMsg
  , recvMsg
  , sendRecv
  ) where

import Control.Exception
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.MessageCounter
import Language.JavaScript.Inline.Transport.Process
import Language.JavaScript.Inline.Transport.Type
import Language.JavaScript.Inline.Transport.Utils
import qualified Paths_inline_js_core
import System.FilePath
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
              { procPath = "node"
              , procArgs =
                  [ "--experimental-modules"
                  , _datadir </> "jsbits" </> "eval.mjs"
                  ]
              , procStdErrInherit = False
              }
        }

data JSSession = JSSession
  { nodeTransport :: Transport
  , msgCounter :: MsgCounter
  , msgRecv :: MsgId -> IO LBS.ByteString
  }

newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  t0 <- newProcessTransport nodeProcessTransportOpts
  t1 <- lockSend $ strictTransport t0
  (_uniq_recv, t2) <-
    uniqueRecv
      (\buf ->
         case runGetOrFail (fromIntegral <$> getWord32le) buf of
           Right (_, _, r) -> Just r
           _ -> Nothing)
      t1
  _msg_counter <- newMsgCounter
  pure
    JSSession
      { nodeTransport = t2
      , msgCounter = _msg_counter
      , msgRecv = coerce _uniq_recv
      }

closeJSSession :: JSSession -> IO ()
closeJSSession JSSession {..} = closeTransport nodeTransport

withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (newJSSession opts) closeJSSession

sendMsg :: JSSession -> SendMsg -> IO MsgId
sendMsg JSSession {..} msg = do
  msg_id <- newMsgId msgCounter
  sendData nodeTransport $ encodeSendMsg msg_id msg
  pure msg_id

recvMsg :: JSSession -> MsgId -> IO RecvMsg
recvMsg JSSession {..} msg_id = do
  buf <- msgRecv msg_id
  case decodeRecvMsg buf of
    Left err ->
      fail $
      "Language.JavaScript.Inline.Session.recvMsg: parsing RecvMsg failed with " <>
      err
    Right (_, msg) -> pure msg

sendRecv :: JSSession -> SendMsg -> IO RecvMsg
sendRecv s = recvMsg s <=< sendMsg s
