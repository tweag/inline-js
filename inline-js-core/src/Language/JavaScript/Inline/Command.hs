{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Language.JavaScript.Inline.Command
  ( eval
  , evalTo
  , evalAsync
  , evalAsyncTo
  ) where

import Control.Monad.Fail
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message.Class
import Language.JavaScript.Inline.Message.Eval
import Language.JavaScript.Inline.Session
import Prelude hiding (fail)

checkEvalResponse :: EvalResponse r -> IO r
checkEvalResponse r =
  case r of
    EvalError {..} ->
      fail $
      "Language.JavaScript.Inline.Commands.checkEvalResponse: evaluation failed with " <>
      show evalError
    EvalResult {..} -> pure evalResult

eval :: Response (EvalResponse r) => JSSession -> JSCode -> IO r
eval s c =
  sendRecv
    s
    EvalRequest
      { evalCode = c
      , evalTimeout = Nothing
      , resolveTimeout = Nothing
      , isAsync = False
      } >>=
  checkEvalResponse

evalTo ::
     Response (EvalResponse r)
  => (r -> Either String a)
  -> JSSession
  -> JSCode
  -> IO a
evalTo p s c = do
  v <- eval s c
  case p v of
    Left err -> fail err
    Right r -> pure r

evalAsync :: Response (EvalResponse r) => JSSession -> JSCode -> IO r
evalAsync s c =
  sendRecv
    s
    EvalRequest
      { evalCode = c
      , evalTimeout = Nothing
      , resolveTimeout = Nothing
      , isAsync = True
      } >>=
  checkEvalResponse

evalAsyncTo ::
     Response (EvalResponse r)
  => (r -> Either String a)
  -> JSSession
  -> JSCode
  -> IO a
evalAsyncTo p s c = do
  v <- evalAsync s c
  case p v of
    Left err -> fail err
    Right r -> pure r
