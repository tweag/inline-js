{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Session
  ( JSSessionOpts(..)
  , defJSSessionOpts
  , JSSession
  , startJSSession
  , killJSSession
  , withJSSession
  , sendMsg
  , recvMsg
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad hiding (fail)
import Control.Monad.Fail
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Coerce
import Data.Functor
import qualified Data.IntMap.Strict as IntMap
import Language.JavaScript.Inline.Async
import qualified Language.JavaScript.Inline.JSON as JSON
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.MessageCounter
import qualified Paths_inline_js
import Prelude hiding (fail)
import System.Environment.Blank
import System.FilePath
import System.IO
import System.Process

data JSSessionOpts = JSSessionOpts
  { nodePath :: FilePath
  , nodeExtraArgs :: [String]
  , nodePwd :: Maybe FilePath
  , nodeExtraEnv :: [(String, String)]
  , nodeStdErrCopyBack :: Bool
  } deriving (Show)

defJSSessionOpts :: JSSessionOpts
defJSSessionOpts =
  JSSessionOpts
    { nodePath = "node"
    , nodeExtraArgs = []
    , nodePwd = Nothing
    , nodeExtraEnv = []
    , nodeStdErrCopyBack = False
    }

data JSSession = JSSession
  { nodeStdIn, nodeStdOut :: Handle
  , nodeStdErr :: Maybe Handle
  , nodeProc :: ProcessHandle
  , msgCounter :: MsgCounter
  , sendQueue :: TQueue (MsgId, SendMsg)
  , recvMap :: TVar (IntMap.IntMap RecvMsg)
  , sendAsync, recvAsync :: Async ()
  }

startJSSession :: JSSessionOpts -> IO JSSession
startJSSession JSSessionOpts {..} = do
  _datadir <- Paths_inline_js.getDataDir
  _env <- getEnvironment
  (Just _stdin, Just _stdout, _m_stderr, _h) <-
    createProcess
      (proc nodePath (nodeExtraArgs <> [_datadir </> "jsbits" </> "server.js"]))
        { cwd = nodePwd
        , env = Just (nodeExtraEnv <> _env)
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err =
            if nodeStdErrCopyBack
              then Inherit
              else CreatePipe
        }
  hSetBinaryMode _stdin True
  hSetBinaryMode _stdout True
  hSetBuffering _stdin LineBuffering
  (0, Result {isError = False, result = JSON.Null}) <- unsafeRecvMsg _stdout
  _msg_counter <- newMsgCounter
  _send_queue <- newTQueueIO
  _sender <-
    newAsync $
    forever $ do
      (_msg_id, _msg) <- atomically $ readTQueue _send_queue
      unsafeSendMsg _stdin _msg_id _msg
  _recv_map <- newTVarIO IntMap.empty
  _recver <-
    newAsync $
    forever $ do
      (_msg_id, _msg) <- unsafeRecvMsg _stdout
      atomically $ modifyTVar' _recv_map $ IntMap.insert (coerce _msg_id) _msg
  pure
    JSSession
      { nodeStdIn = _stdin
      , nodeStdOut = _stdout
      , nodeStdErr = _m_stderr
      , nodeProc = _h
      , msgCounter = _msg_counter
      , sendQueue = _send_queue
      , recvMap = _recv_map
      , sendAsync = _sender
      , recvAsync = _recver
      }

killJSSession :: JSSession -> IO ()
killJSSession JSSession {..} = do
  terminateProcess nodeProc
  killAsync sendAsync
  killAsync recvAsync

withJSSession :: JSSessionOpts -> (JSSession -> IO r) -> IO r
withJSSession opts = bracket (startJSSession opts) killJSSession

unsafeSendMsg :: Handle -> MsgId -> SendMsg -> IO ()
unsafeSendMsg _node_stdin msg_id msg =
  hPutBuilder _node_stdin $ JSON.encode (encodeSendMsg msg_id msg) <> char7 '\n'

unsafeRecvMsg :: Handle -> IO (MsgId, RecvMsg)
unsafeRecvMsg _node_stdout = do
  l <- BS.hGetLine _node_stdout
  case JSON.decode $ LBS.fromStrict l of
    Left err ->
      fail $
      "Language.JavaScript.Inline.JSON.unsafeRecvMsg: parsing Value failed with " <>
      err
    Right v ->
      case decodeRecvMsg v of
        Left err ->
          fail $
          "Language.JavaScript.Inline.JSON.unsafeRecvMsg: parsing RecvMsg failed with " <>
          err
        Right msg -> pure msg

sendMsg :: JSSession -> SendMsg -> IO MsgId
sendMsg JSSession {..} msg = do
  _msg_id <- newMsgId msgCounter
  atomically $ writeTQueue sendQueue (_msg_id, msg)
  pure _msg_id

recvMsg :: JSSession -> MsgId -> IO RecvMsg
recvMsg JSSession {..} msg_id =
  atomically $ do
    _recv_map_prev <- readTVar recvMap
    case IntMap.updateLookupWithKey
           (\_ _ -> Nothing)
           (coerce msg_id)
           _recv_map_prev of
      (Just _msg, _recv_map) -> writeTVar recvMap _recv_map $> _msg
      _ -> retry
