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
import Language.JavaScript.Inline.Message.Class
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
  (_uniq_recv, t2) <- uniqueRecv (runGet (fromIntegral <$> getWord32le)) t1
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

sendMsg :: Message i o => JSSession -> i -> IO MsgId
sendMsg JSSession {..} msg = do
  msg_id <- newMsgId msgCounter
  sendData nodeTransport $ encodeRequest msg_id msg
  pure msg_id

recvMsg :: Message i o => JSSession -> MsgId -> IO o
recvMsg JSSession {..} msg_id = do
  buf <- msgRecv msg_id
  decodeResponse msg_id buf

sendRecv :: Message i o => JSSession -> i -> IO o
sendRecv s = recvMsg s <=< sendMsg s
