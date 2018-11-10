module Language.JavaScript.Inline.JSCode
  ( JSCode
  , codeToString
  , codeFromValue
  , JSRefRegion
  , valueFromJSRefRegion
  , parseJSRefRegion
  , JSRef
  , parseJSRef
  , newJSRefRegion
  , freeJSRefRegion
  , deRefJSRef
  , asyncify
  ) where

import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import qualified Language.JavaScript.Inline.JSON as JSON

type JSCode = Builder

codeToString :: JSCode -> JSON.JSString
codeToString = LText.toStrict . toLazyText

codeFromValue :: JSON.Value -> JSCode
codeFromValue v = fromString "(()=>(" <> JSON.encode v <> fromString "))()"

newtype JSRefRegion =
  JSRefRegion Int
  deriving (Eq, Ord, Show)

valueFromJSRefRegion :: JSRefRegion -> JSON.Value
valueFromJSRefRegion (JSRefRegion r) = JSON.Number $ fromIntegral r

parseJSRefRegion :: JSON.Value -> Either String JSRefRegion
parseJSRefRegion v =
  case v of
    JSON.Number r -> Right $ JSRefRegion $ truncate r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSRefRegion"

newtype JSRef =
  JSRef Int
  deriving (Eq, Ord, Show)

parseJSRef :: JSON.Value -> Either String JSRef
parseJSRef v =
  case v of
    JSON.Number r -> Right $ JSRef $ truncate r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSRef"

newJSRefRegion :: JSCode
newJSRefRegion = fromString "JSRef.newJSRefRegion()"

freeJSRefRegion :: JSRefRegion -> JSCode
freeJSRefRegion (JSRefRegion r) =
  fromString "JSRef.freeJSRefRegion(0x" <> hexadecimal r <> singleton ')'

deRefJSRef :: JSRefRegion -> JSRef -> JSCode
deRefJSRef (JSRefRegion r) (JSRef p) =
  fromString "JSRef.deRefJSRef(0x" <> hexadecimal r <> fromString ",0x" <>
  hexadecimal p <>
  fromString ")"

asyncify :: JSCode -> JSCode
asyncify expr = fromString "(async () => (" <> expr <> fromString "))()"
