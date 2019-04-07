{-# LANGUAGE FlexibleInstances #-}
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
import Data.Word
import qualified Language.JavaScript.Inline.JSCode as JSCode
import Language.JavaScript.Inline.Message.Class

data EvalRequest a = EvalRequest
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Int
  , evalCode :: JSCode.JSCode
  }

data EvalResponse a
  = EvalError { evalError :: LBS.ByteString }
  | EvalResult { evalResult :: a }

instance Request (EvalRequest LBS.ByteString) where
  putRequest = putRequestWith 0

instance Request (EvalRequest JSCode.JSVal) where
  putRequest = putRequestWith 1

instance Response (EvalResponse LBS.ByteString) where
  getResponse = getResponseWith getRemainingLazyByteString

instance Response (EvalResponse JSCode.JSVal) where
  getResponse = getResponseWith (JSCode.JSVal . fromIntegral <$> getWord32host)

putRequestWith :: Word32 -> EvalRequest a -> Put
putRequestWith rt EvalRequest {..} = do
  putWord32host 0
  putWord32host rt
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

getResponseWith :: Get r -> Get (EvalResponse r)
getResponseWith p = do
  is_err <- getWord32host
  case is_err of
    0 -> EvalResult <$> p
    _ -> EvalError <$> getRemainingLazyByteString
