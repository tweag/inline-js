{-# LANGUAGE RecordWildCards #-}

module Language.JavaScript.Inline.Command
  ( eval
  , evalTo
  , evalAsync
  , evalAsyncTo
  ) where

import Control.Monad.Fail
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message.Eval
import Language.JavaScript.Inline.Session
import Prelude hiding (fail)

checkEvalResponse :: EvalResponse -> IO LBS.ByteString
checkEvalResponse EvalResponse {..} =
  if isError
    then fail $
         "Language.JavaScript.Inline.Commands.checkEvalResponse: evaluation failed with " <>
         show result
    else pure result

eval :: JSSession -> JSCode -> IO LBS.ByteString
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

evalTo :: (LBS.ByteString -> Either String a) -> JSSession -> JSCode -> IO a
evalTo p s c = do
  v <- eval s c
  case p v of
    Left err -> fail err
    Right r -> pure r

evalAsync :: JSSession -> JSCode -> IO LBS.ByteString
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
     (LBS.ByteString -> Either String a) -> JSSession -> JSCode -> IO a
evalAsyncTo p s c = do
  v <- evalAsync s c
  case p v of
    Left err -> fail err
    Right r -> pure r
