{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode(..)
  , codeToString
  , codeFromString
  , codeFromValueLBS
  , jsonStringify
  , JSVal(..)
  , deRefJSVal
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

newtype JSCode =
  JSCode Builder
  deriving (IsString, Semigroup)

unwrap :: JSCode -> Builder
unwrap (JSCode builder) = builder

codeFromString :: Text -> JSCode
codeFromString = JSCode . byteString . Text.encodeUtf8

codeToString :: JSCode -> Text
codeToString = Text.decodeUtf8 . LBS.toStrict . toLazyByteString . unwrap

codeFromValueLBS :: LBS.ByteString -> JSCode
codeFromValueLBS buf =
  JSCode $
  mconcat [fromString "JSON.parse(", lazyByteString buf, fromString ")"]

jsonStringify :: JSCode -> JSCode
jsonStringify expr = "JSON.stringify(" <> expr <> ")"

newtype JSVal =
  JSVal Int
  deriving (Eq, Ord, Show)

deRefJSVal :: JSVal -> JSCode
deRefJSVal (JSVal p) =
  JSCode $ mconcat [fromString "JSVal.deRefJSVal(", intDec p, fromString ")"]
