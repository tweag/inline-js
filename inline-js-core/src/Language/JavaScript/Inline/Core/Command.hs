{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Command
  ( eval
  , alloc
  , importMJS
  ) where

import Control.Monad.Fail
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core.JSCode
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.Message.Eval
import Language.JavaScript.Inline.Core.Session
import Prelude hiding (fail)
import System.Directory

checkEvalResponse :: EvalResponse r -> IO r
checkEvalResponse r =
  case r of
    EvalError {..} ->
      fail $
      "Language.JavaScript.Inline.Core.Commands.checkEvalResponse: evaluation failed with " <>
      show evalError
    EvalResult {..} -> pure evalResult

eval ::
     forall r.
     ( Request (EvalRequest r)
     , Response (EvalResponse r)
     , ResponseOf (EvalRequest r) ~ (EvalResponse r)
     )
  => JSSession
  -> JSCode
  -> IO r
eval s c =
  sendRecv
    s
    (EvalRequest {evalTimeout = Nothing, resolveTimeout = Nothing, evalCode = c} :: EvalRequest r) >>=
  checkEvalResponse

alloc :: JSSession -> LBS.ByteString -> IO JSVal
alloc s buf = sendRecv s AllocRequest {allocContent = buf} >>= checkEvalResponse

importMJS :: JSSession -> FilePath -> IO JSVal
importMJS s p = do
  p' <- canonicalizePath p
  sendRecv s ImportRequest {importPath = p'} >>= checkEvalResponse
