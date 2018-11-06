{-# LANGUAGE RecordWildCards #-}

module Language.JavaScript.Inline.Command
  ( eval
  , evalJSRef
  , evalTo
  , evalAsync
  , evalAsyncJSRef
  , evalAsyncTo
  ) where

import Control.Monad.Fail
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session
import Prelude hiding (fail)

checkRecvMsg :: RecvMsg -> IO Value
checkRecvMsg Result {..} =
  if isError
    then fail $
         "Language.JavaScript.Inline.Commands.checkRecvMsg: evaluation failed with " <>
         show result
    else pure result

eval :: JSSession -> JSCode -> IO Value
eval s c =
  sendRecv
    s
    Eval
      { evalCode = c
      , evalTimeout = Nothing
      , resolveTimeout = Nothing
      , isAsync = False
      , region = Nothing
      } >>=
  checkRecvMsg

evalJSRef :: JSSession -> JSRefRegion -> JSCode -> IO JSRef
evalJSRef s r c = do
  v <-
    sendRecv
      s
      Eval
        { evalCode = c
        , evalTimeout = Nothing
        , resolveTimeout = Nothing
        , isAsync = False
        , region = Just r
        } >>=
    checkRecvMsg
  case parseJSRef v of
    Left err -> fail err
    Right p -> pure p

evalTo :: (Value -> Either String a) -> JSSession -> JSCode -> IO a
evalTo p s c = do
  v <- eval s c
  case p v of
    Left err -> fail err
    Right r -> pure r

evalAsync :: JSSession -> JSCode -> IO Value
evalAsync s c =
  sendRecv
    s
    Eval
      { evalCode = c
      , evalTimeout = Nothing
      , resolveTimeout = Nothing
      , isAsync = True
      , region = Nothing
      } >>=
  checkRecvMsg

evalAsyncJSRef :: JSSession -> JSRefRegion -> JSCode -> IO JSRef
evalAsyncJSRef s r c = do
  v <-
    sendRecv
      s
      Eval
        { evalCode = c
        , evalTimeout = Nothing
        , resolveTimeout = Nothing
        , isAsync = True
        , region = Just r
        } >>=
    checkRecvMsg
  case parseJSRef v of
    Left err -> fail err
    Right p -> pure p

evalAsyncTo :: (Value -> Either String a) -> JSSession -> JSCode -> IO a
evalAsyncTo p s c = do
  v <- evalAsync s c
  case p v of
    Left err -> fail err
    Right r -> pure r
