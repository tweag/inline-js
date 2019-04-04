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
import Data.Coerce
import Data.IORef
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
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
  , recvMap :: IORef (IntMap RecvMsg)
  }

newJSSession :: JSSessionOpts -> IO JSSession
newJSSession JSSessionOpts {..} = do
  t <- newProcessTransport nodeProcessTransportOpts
  t' <- lockSend t
  _msg_counter <- newMsgCounter
  _recv_map <- newIORef IntMap.empty
  pure
    JSSession
      { nodeTransport = strictTransport t'
      , msgCounter = _msg_counter
      , recvMap = _recv_map
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
recvMsg JSSession {..} msg_id = w
  where
    w = do
      r <-
        atomicModifyIORef' recvMap $ \m ->
          let (r, m') =
                IntMap.updateLookupWithKey (\_ _ -> Nothing) (coerce msg_id) m
           in (m', r)
      case r of
        Just msg -> pure msg
        _ -> do
          buf <- recvData nodeTransport
          (msg_id', msg') <-
            case decodeRecvMsg buf of
              Left err ->
                fail $
                "Language.JavaScript.Inline.Session.recvMsg: parsing RecvMsg failed with " <>
                err
              Right msg -> pure msg
          atomicModifyIORef' recvMap $ \m ->
            (IntMap.insert (coerce msg_id') msg' m, ())
          w

sendRecv :: JSSession -> SendMsg -> IO RecvMsg
sendRecv s = recvMsg s <=< sendMsg s
