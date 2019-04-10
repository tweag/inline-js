{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message.Eval
  ( EvalRequest(..)
  , AllocRequest(..)
  , ImportRequest(..)
  , EvalResponse(..)
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.Word
import qualified Language.JavaScript.Inline.JSCode as JSCode
import Language.JavaScript.Inline.Message.Class

data EvalRequest a = EvalRequest
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Int
  , evalCode :: JSCode.JSCode
  }

newtype AllocRequest = AllocRequest
  { allocContent :: LBS.ByteString
  }

newtype ImportRequest = ImportRequest
  { importPath :: FilePath
  }

data EvalResponse a
  = EvalError { evalError :: LBS.ByteString }
  | EvalResult { evalResult :: a }

instance Request (EvalRequest LBS.ByteString) where
  putRequest = putEvalRequestWith 0

instance Request (EvalRequest JSCode.JSVal) where
  putRequest = putEvalRequestWith 1

instance Request (EvalRequest ()) where
  putRequest = putEvalRequestWith 2

instance Request AllocRequest where
  putRequest AllocRequest {..} = do
    putWord32host 1
    putLazyByteString allocContent

instance Request ImportRequest where
  putRequest ImportRequest {..} = do
    putWord32host 2
    putLazyByteString $ LText.encodeUtf8 $ LText.pack importPath

instance Response (EvalResponse LBS.ByteString) where
  getResponse = getResponseWith getRemainingLazyByteString

instance Response (EvalResponse JSCode.JSVal) where
  getResponse = getResponseWith (JSCode.JSVal . fromIntegral <$> getWord32host)

instance Response (EvalResponse ()) where
  getResponse = getResponseWith $ pure ()

putEvalRequestWith :: Word32 -> EvalRequest a -> Put
putEvalRequestWith rt EvalRequest {..} = do
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
