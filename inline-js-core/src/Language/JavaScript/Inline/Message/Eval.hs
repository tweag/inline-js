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

instance Request (EvalRequest a) where
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

instance Response (EvalResponse LBS.ByteString) where
  getResponse = getResponseWith getRemainingLazyByteString

instance Response (EvalResponse JSCode.JSVal) where
  getResponse = getResponseWith (JSCode.JSVal . fromIntegral <$> getWord32host)

getResponseWith :: Get r -> Get (EvalResponse r)
getResponseWith p = do
  is_err <- getWord32host
  case is_err of
    0 -> EvalResult <$> p
    _ -> EvalError <$> getRemainingLazyByteString
