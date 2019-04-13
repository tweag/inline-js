{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Message.Eval
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
import qualified Language.JavaScript.Inline.Core.JSCode as JSCode
import Language.JavaScript.Inline.Core.Message.Class

-- | Request to run a 'JSCode.JSCode'.
--
-- The code is evaluated to a @Promise@ first (if not, the result is called with @Promise.resolve()@ anyway),
-- then we wait for the @Promise@ to resolve/reject, and finally return the result.
--
-- It's possible to specify evaluate/resolve timeout in milliseconds.
--
-- When parameterised by 'LBS.ByteString', the result is wrapped by @Buffer.from()@ and returned.
--
-- When parameterised by 'JSCode.JSVal', the 'JSCode.JSVal' associated with the result is returned.
--
-- When parameterised by '()', the result is discarded.
data EvalRequest a = EvalRequest
  { evalTimeout, resolveTimeout :: Maybe Int
  , evalCode :: JSCode.JSCode
  }

-- | Request to allocate a @Buffer@.
--
-- Returns a 'JSCode.JSVal'.
newtype AllocRequest = AllocRequest
  { allocContent :: LBS.ByteString
  }

-- | Request to @import()@ a @.mjs@ ECMAScript module.
--
-- Returns a 'JSCode.JSVal' of the module namespace object.
newtype ImportRequest = ImportRequest
  { importPath :: FilePath
  }

-- | The response type of all requests.
--
-- Please make sure the type parameter matches requirement of the request type (as described in their docs),
-- otherwise undefined behavior awaits!
data EvalResponse a
  = EvalError { evalError :: LBS.ByteString }
  | EvalResult { evalResult :: a }

instance Request (EvalRequest LBS.ByteString) where
  type ResponseOf (EvalRequest LBS.ByteString) = EvalResponse LBS.ByteString
  putRequest = putEvalRequestWith 0

instance Request (EvalRequest JSCode.JSVal) where
  type ResponseOf (EvalRequest JSCode.JSVal) = EvalResponse JSCode.JSVal
  putRequest = putEvalRequestWith 1

instance Request (EvalRequest ()) where
  type ResponseOf (EvalRequest ()) = EvalResponse ()
  putRequest = putEvalRequestWith 2

instance Request AllocRequest where
  type ResponseOf AllocRequest = EvalResponse JSCode.JSVal
  putRequest AllocRequest {..} = do
    putWord32host 1
    putLazyByteString allocContent

instance Request ImportRequest where
  type ResponseOf ImportRequest = EvalResponse JSCode.JSVal
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
