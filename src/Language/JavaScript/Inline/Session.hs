{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Language.JavaScript.Inline.Session
  ( JSSource
  , Session
  , EvalException(..)
  , eval
  , closeSession
  , newSession
  ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Language.JavaScript.Inline.Configure
import Language.JavaScript.Inline.Internals
import Network.WebSockets
import System.Environment
import System.IO
import System.Process
import UnliftIO

type JSSource = T.Text

data Session = Session
  { eval :: forall m a. (MonadIO m, FromJSON a) =>
                          JSSource -> m a
  , closeSession :: forall m. MonadIO m =>
                                m ()
  }

data EvalReq = EvalReq
  { id :: MsgId
  , code :: JSSource
  }

$(deriveToJSON defaultOptions ''EvalReq)

data EvalResp
  = EvalResult { id :: MsgId
               , result :: Value }
  | EvalError { id :: MsgId
              , error :: Value }

$(deriveFromJSON defaultOptions {sumEncoding = UntaggedValue} ''EvalResp)

data EvalException
  = EvalFailed { code :: JSSource
               , error :: Value }
  | ResultDecodingFailed { code :: JSSource
                         , decodingError :: String }
  deriving (Show)

instance Exception EvalException

newSession :: MonadIO m => ConfigureOptions -> m Session
newSession ConfigureOptions {..} = do
  current_env <- liftIO getEnvironment
  (_, Just h_stdout, _, h_node) <-
    liftIO $
    createProcess
      (proc "node" nodeArgs)
        { cwd = Just jsbitsPath
        , env = Just $ additionalEnv ++ current_env
        , std_out = CreatePipe
        }
  node_port <- fmap read $ liftIO $ hGetLine h_stdout
  conn_ref <- newEmptyMVar
  chan_pool <- newIORef HM.empty
  ws_t <-
    liftIO $
    async $
    runClient "127.0.0.1" node_port "" $ \conn -> do
      putMVar conn_ref conn
      forever $ do
        msg_buf <- receiveData conn
        let Just eval_resp = decode' msg_buf
            resp_id =
              case eval_resp of
                EvalResult {id = x} -> x
                EvalError {id = x} -> x
        resp_chan <-
          atomicModifyIORef' chan_pool $ \cp ->
            (HM.delete resp_id cp, cp HM.! resp_id)
        putMVar resp_chan eval_resp
  pure
    Session
      { eval =
          \js_src -> do
            conn <- readMVar conn_ref
            msg_id <- newMsgId
            resp_chan <- newEmptyMVar
            atomicModifyIORef' chan_pool $ \cp ->
              (HM.insert msg_id resp_chan cp, ())
            liftIO $
              sendBinaryData conn $ encode EvalReq {id = msg_id, code = js_src}
            resp_v <- takeMVar resp_chan
            case resp_v of
              EvalResult _ v ->
                case fromJSON v of
                  Error err -> throwIO $ ResultDecodingFailed js_src err
                  Success r -> pure r
              EvalError _ v -> throwIO $ EvalFailed js_src v
      , closeSession =
          do liftIO $ terminateProcess h_node
             cancel ws_t
      }
