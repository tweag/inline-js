{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline.Core.JSCode
  ( JSCode (..),
    bufferToString,
    jsonParse,
    jsonStringify,
    importMJS,
    JSVal (..),
    deRefJSVal,
    freeJSVal,
    takeJSVal,
  )
where

import Data.ByteString.Builder
import Data.Coerce
import Data.String (IsString (..))

-- | A UTF-8 encoded JavaScript code snippet. Can be a single expression or a
-- series of statements.
--
-- Larger expressions can be easily composed from smaller ones by concating
-- 'JSCode', and occasionally adding brackets to ensure valid syntax (e.g.
-- @(f)(a)@ when @f@ is a function expression).
newtype JSCode
  = JSCode Builder
  deriving (IsString, Semigroup, Monoid)

instance Show JSCode where
  showsPrec p = showsPrec p . toLazyByteString . coerce

-- | UTF-8 decode a @Buffer@ and return a @string@.
bufferToString :: JSCode -> JSCode
bufferToString expr =
  "(new TextDecoder('utf-8', {fatal: true})).decode(" <> expr <> ")"

-- | @JSON.parse()@ a @string@.
jsonParse :: JSCode -> JSCode
jsonParse expr = "JSON.parse(" <> expr <> ")"

-- | @JSON.stringify()@ a value.
jsonStringify :: JSCode -> JSCode
jsonStringify expr = "JSON.stringify(" <> expr <> ")"

importMJS :: FilePath -> JSCode
importMJS p =
  coerce $
    "import('url').then(url => url.pathToFileURL("
      <> stringUtf8 (show p)
      <> ").href).then(url => import(url))"

-- | A reference to a JavaScript value.
--
-- It's supposed to be completely opaque, yet we expose its internal here, so
-- you can use 'JSVal's in stuff like 'Data.IntMap.IntMap'.
--
-- You can't make a valid 'JSVal' out of thin air. Obtain it either via
-- higher-level TH splices in @inline-js@, or via something like
-- 'Language.JavaScript.Inline.Core.eval', explicitly annotating the return
-- type.
newtype JSVal
  = JSVal Int
  deriving (Eq, Ord, Show)

-- | Dereferences a 'JSVal' and returns the value.
--
-- Throws on a non-existent 'JSVal'.
deRefJSVal :: JSVal -> JSCode
deRefJSVal (JSVal p) = JSCode $ mconcat ["JSVal.deRefJSVal(", intDec p, ")"]

-- | Removes a 'JSVal' and returns nothing.
--
-- Don't forget to call it on unused 'JSVal's when using a long-lived
-- 'Language.JavaScript.Inline.Core.JSSession'.
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
