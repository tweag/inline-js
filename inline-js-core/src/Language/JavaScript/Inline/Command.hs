{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.JavaScript.Inline.Command
  ( eval
  , evalAsync
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

eval ::
     forall r. (Request (EvalRequest r), Response (EvalResponse r))
  => JSSession
  -> JSCode
  -> IO r
eval s c =
  sendRecv
    s
    (EvalRequest
       { evalCode = c
       , evalTimeout = Nothing
       , resolveTimeout = Nothing
       , isAsync = False
       } :: EvalRequest r) >>=
  checkEvalResponse

evalAsync ::
     forall r. (Request (EvalRequest r), Response (EvalResponse r))
  => JSSession
  -> JSCode
  -> IO r
evalAsync s c =
  sendRecv
    s
    (EvalRequest
       { evalCode = c
       , evalTimeout = Nothing
       , resolveTimeout = Nothing
       , isAsync = True
       } :: EvalRequest r) >>=
  checkEvalResponse
