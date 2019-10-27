{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.JavaScript.Inline.Class
  ( toJSCode,
    withFromEvalResult,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Base64.Lazy as LBS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.Coerce
import Data.Proxy
import Language.JavaScript.Inline.Core

class ToJSCode a where
  toJSCode :: a -> JSCode

instance ToJSCode BS.ByteString where
  {-# INLINE toJSCode #-}
  toJSCode buf =
    "Buffer.from('" <> coerce (byteString (BS.encode buf)) <> "', 'base64')"

instance ToJSCode LBS.ByteString where
  {-# INLINE toJSCode #-}
  toJSCode buf =
    "Buffer.from('"
      <> coerce (lazyByteString (LBS.encode buf))
      <> "', 'base64')"

instance ToJSCode SBS.ShortByteString where
  {-# INLINE toJSCode #-}
  toJSCode = toJSCode . SBS.fromShort

instance ToJSCode JSVal where
  {-# INLINE toJSCode #-}
  toJSCode = deRefJSVal

instance {-# OVERLAPPABLE #-} Aeson.ToJSON a => ToJSCode a where
  {-# INLINE toJSCode #-}
  toJSCode = coerce . Aeson.fromEncoding . Aeson.toEncoding

type family EvalResult a where
  EvalResult JSVal = JSVal
  EvalResult () = ()
  EvalResult a = LBS.ByteString

class FromEvalResult a where

  postProcessor :: proxy a -> JSCode
  {-# INLINE postProcessor #-}
  postProcessor _ = "r => r"

  fromEvalResult :: EvalResult a -> Either String a

instance FromEvalResult BS.ByteString where
  {-# INLINE fromEvalResult #-}
  fromEvalResult = Right . LBS.toStrict

instance FromEvalResult LBS.ByteString where
  {-# INLINE fromEvalResult #-}
  fromEvalResult = Right

instance FromEvalResult SBS.ShortByteString where
  {-# INLINE fromEvalResult #-}
  fromEvalResult = Right . SBS.toShort . LBS.toStrict

instance FromEvalResult JSVal where
  {-# INLINE fromEvalResult #-}
  fromEvalResult = Right

instance FromEvalResult () where
  {-# INLINE fromEvalResult #-}
  fromEvalResult = Right

instance
  {-# OVERLAPPABLE #-}
  (EvalResult a ~ LBS.ByteString, Aeson.FromJSON a) =>
  FromEvalResult a
  where

  {-# INLINE postProcessor #-}
  postProcessor _ = "r => JSON.stringify(r)"

  {-# INLINE fromEvalResult #-}
  fromEvalResult = Aeson.eitherDecode'

{-# INLINE withFromEvalResult #-}
withFromEvalResult ::
  forall a r.
  FromEvalResult a =>
  (JSCode -> (EvalResult a -> Either String a) -> r) ->
  r
withFromEvalResult c = c (postProcessor (Proxy :: Proxy a)) fromEvalResult
