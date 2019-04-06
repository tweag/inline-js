{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message.Eval
  ( EvalRequest(..)
  , EvalResponse(..)
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Language.JavaScript.Inline.JSCode as JSCode
import Language.JavaScript.Inline.Message.Class

data EvalRequest = EvalRequest
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Int
  , evalCode :: JSCode.JSCode
  }

data EvalResponse
  = EvalError { evalError :: LBS.ByteString }
  | EvalResult { evalResult :: LBS.ByteString }

instance Message EvalRequest EvalResponse where
  putRequest EvalRequest {..} = do
    putWord32host 0
    putWord32host $
      if isAsync
        then 1
        else 0
    putWord32host $
      fromIntegral $
      case evalTimeout of
        Just t -> t
        _ -> 0
    putWord32host $
      fromIntegral $
      case resolveTimeout of
        Just t -> t
        _ -> 0
    putBuilder $ coerce evalCode
  getResponse = do
    is_err <- getWord32host
    r <- getRemainingLazyByteString
    pure $
      case is_err of
        0 -> EvalResult {evalResult = r}
        _ -> EvalError {evalError = r}
