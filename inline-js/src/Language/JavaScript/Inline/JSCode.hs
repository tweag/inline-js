module Language.JavaScript.Inline.JSCode
  ( JSCode
  , codeToString
  , JSRefRegion
  , JSRef
  , newJSRefRegion
  , freeJSRefRegion
  , newJSRef
  , deRefJSRef
  ) where

import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import qualified Language.JavaScript.Inline.JSON as JSON

type JSCode = Builder

codeToString :: JSCode -> JSON.JSString
codeToString = LText.toStrict . toLazyText

newtype JSRefRegion =
  JSRefRegion Int
  deriving (Eq, Ord, Show)

newtype JSRef =
  JSRef Int
  deriving (Eq, Ord, Show)

newJSRefRegion :: JSCode
newJSRefRegion = fromString "JSRef.newJSRefRegion()"

freeJSRefRegion :: JSRefRegion -> JSCode
freeJSRefRegion (JSRefRegion r) =
  fromString "JSRef.freeJSRefRegion(0x" <> hexadecimal r <> singleton ')'

newJSRef :: JSRefRegion -> JSCode -> JSCode
newJSRef (JSRefRegion r) expr =
  fromString "JSRef.newJSRef(0x" <> hexadecimal r <> fromString ",(" <> expr <>
  fromString "))"

deRefJSRef :: JSRefRegion -> JSRef -> JSCode
deRefJSRef (JSRefRegion r) (JSRef p) =
  fromString "JSRef.deRefJSRef(0x" <> hexadecimal r <> fromString ",0x" <>
  hexadecimal p <>
  fromString ")"
