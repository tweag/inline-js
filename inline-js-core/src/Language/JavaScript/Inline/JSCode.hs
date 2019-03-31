{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode
  , codeToString
  , codeFromString
  , codeFromValue
  , JSRef
  , parseJSRef
  , newJSRef
  , deRefJSRef
  ) where

import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import qualified Language.JavaScript.Inline.JSON as JSON

newtype JSCode =
  JSCode Builder
  deriving (Show, IsString)

unwrap :: JSCode -> Builder
unwrap (JSCode builder) = builder

codeFromString :: Text -> JSCode
codeFromString = JSCode . fromText

codeToString :: JSCode -> Text
codeToString = LText.toStrict . toLazyText . unwrap

codeFromValue :: JSON.Value -> JSCode
codeFromValue v =
  JSCode $
  mconcat
    [ fromString "JSON.parse("
    , JSON.encode $ JSON.String $ JSON.encodeText v
    , singleton ')'
    ]

newtype JSRef =
  JSRef Int
  deriving (Eq, Ord, Show)

parseJSRef :: JSON.Value -> Either String JSRef
parseJSRef v =
  case v of
    JSON.Number r -> Right $ JSRef $ truncate r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSRef"

newJSRef :: JSCode -> JSCode
newJSRef expr =
  JSCode $ mconcat [fromString "JSRef.newJSRef((", unwrap expr, fromString "))"]

deRefJSRef :: JSRef -> JSCode
deRefJSRef (JSRef p) =
  JSCode $
  mconcat [fromString "JSRef.deRefJSRef(0x", hexadecimal p, fromString ")"]
