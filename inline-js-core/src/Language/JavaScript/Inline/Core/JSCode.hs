{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline.Core.JSCode
  ( JSCode(..)
  , codeToString
  , bufferToString
  , jsonParse
  , jsonStringify
  , JSVal(..)
  , deRefJSVal
  , freeJSVal
  , takeJSVal
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- | A UTF-8 encoded JavaScript code snippet.
--
-- Can be a single expression or a series of statements.
newtype JSCode =
  JSCode Builder
  deriving (IsString, Semigroup, Monoid)

instance Show JSCode where
  showsPrec p = showsPrec p . toLazyByteString . unwrap

unwrap :: JSCode -> Builder
unwrap = coerce

codeToString :: JSCode -> Text
codeToString = Text.decodeUtf8 . LBS.toStrict . toLazyByteString . unwrap

-- | UTF-8 decode a @Buffer@.
bufferToString :: JSCode -> JSCode
bufferToString expr =
  "(new TextDecoder('utf-8', {fatal: true})).decode(" <> expr <> ")"

-- | @JSON.parse()@ a string.
jsonParse :: JSCode -> JSCode
jsonParse expr = "JSON.parse(" <> expr <> ")"

-- | @JSON.stringify()@ a value.
jsonStringify :: JSCode -> JSCode
jsonStringify expr = "JSON.stringify(" <> expr <> ")"

-- | An opaque reference to a JavaScript value.
newtype JSVal =
  JSVal Int
  deriving (Eq, Ord, Show)

-- | Dereferences a 'JSVal' and returns the value.
--
-- Throws on a non-existent 'JSVal'.
deRefJSVal :: JSVal -> JSCode
deRefJSVal (JSVal p) = JSCode $ mconcat ["JSVal.deRefJSVal(", intDec p, ")"]

-- | Removes a 'JSVal' and returns nothing.
--
-- Don't forget to call it on unused 'JSVal's when using a long-lived 'Language.JavaScript.Inline.Core.Session.JSSession'.
--
-- Throws on a non-existent 'JSVal'.
freeJSVal :: JSVal -> JSCode
freeJSVal (JSVal p) = JSCode $ mconcat ["JSVal.freeJSVal(", intDec p, ")"]

-- | Removes a 'JSVal' and returns the value.
--
-- Prefer this over 'deRefJSVal' when you're sure the 'JSVal' is used only once.
--
-- Throws on a non-existent 'JSVal'.
takeJSVal :: JSVal -> JSCode
takeJSVal (JSVal p) = JSCode $ mconcat ["JSVal.takeJSVal(", intDec p, ")"]
