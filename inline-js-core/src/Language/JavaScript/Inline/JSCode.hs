{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode(..)
  , codeToString
  , bufferToString
  , jsonParse
  , jsonStringify
  , JSVal(..)
  , deRefJSVal
  , freeJSVal
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

newtype JSCode =
  JSCode Builder
  deriving (IsString, Semigroup, Monoid)

instance Show JSCode where
  showsPrec p = showsPrec p . toLazyByteString . unwrap

unwrap :: JSCode -> Builder
unwrap = coerce

codeToString :: JSCode -> Text
codeToString = Text.decodeUtf8 . LBS.toStrict . toLazyByteString . unwrap

bufferToString, jsonParse, jsonStringify :: JSCode -> JSCode
bufferToString expr =
  "(new TextDecoder('utf-8', {fatal: true})).decode(" <> expr <> ")"

jsonParse expr = "JSON.parse(" <> expr <> ")"

jsonStringify expr = "JSON.stringify(" <> expr <> ")"

newtype JSVal =
  JSVal Int
  deriving (Eq, Ord, Show)

deRefJSVal, freeJSVal :: JSVal -> JSCode
deRefJSVal (JSVal p) = JSCode $ mconcat ["JSVal.deRefJSVal(", intDec p, ")"]

freeJSVal (JSVal p) = JSCode $ mconcat ["JSVal.freeJSVal(", intDec p, ")"]
