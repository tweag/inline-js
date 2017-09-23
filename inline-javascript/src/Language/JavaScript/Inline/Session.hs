{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Session
  ( JSSource
  , Session
  , eval
  , closeSession
  , newSession
  ) where

import Data.Aeson
import qualified Data.Text as T
import Language.JavaScript.Inline.Configure
import Network.HTTP.Client
import Network.HTTP.Types
import System.Environment
import System.IO
import System.Process

type JSSource = T.Text

data Session = Session
  { eval :: forall a. FromJSON a =>
                        JSSource -> IO a
  , closeSession :: IO ()
  }

newSession :: ConfigureOptions -> IO Session
newSession conf_opts@ConfigureOptions {..} = do
  current_env <- getEnvironment
  (_, Just h_stdout, _, h_node) <-
    createProcess
      (proc "node" nodeArgs)
        { cwd = Just jsbitsPath
        , env = Just $ additionalEnv ++ current_env
        , std_out = CreatePipe
        }
  node_port <- read <$> hGetLine h_stdout
  init_req <- parseUrlThrow "http://localhost/eval"
  let req =
        init_req
          { method = methodPost
          , port = node_port
          , requestHeaders =
              [ (hAccept, "application/json")
              , (hContentType, "application/json; charset=utf-8")
              ]
          , cookieJar = Nothing
          }
  mgr <-
    newManager
      defaultManagerSettings {managerResponseTimeout = responseTimeoutNone}
  pure
    Session
      { eval =
          \js_src -> do
            let req' =
                  req {requestBody = RequestBodyLBS $ encode $ String js_src}
            resp <- httpLbs req' mgr
            case eitherDecode' $ responseBody resp of
              Right v -> pure v
              _ ->
                fail $
                "Illegal response from eval server.\nConfigureOptions: " ++
                show conf_opts ++
                "\nRequest: " ++ show req' ++ "\nResponse: " ++ show resp
      , closeSession = terminateProcess h_node
      }
