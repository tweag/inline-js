{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode
  , codeToString
  , codeFromString
  , codeFromValueLBS
  , JSRef
  , parseJSRef
  , newJSRef
  , deRefJSRef
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import qualified Data.Text.Read as Text
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

codeFromValueLBS :: LBS.ByteString -> JSCode
codeFromValueLBS buf =
  JSCode $
  mconcat
    [ fromString "JSON.parse("
    , JSON.encode $ JSON.String $ Text.decodeUtf8 $ LBS.toStrict buf
    , singleton ')'
    ]

newtype JSRef =
  JSRef Int
  deriving (Eq, Ord, Show)

parseJSRef :: Text -> Either String JSRef
parseJSRef v =
  case Text.decimal v of
    Right (r, _) -> Right $ JSRef r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSRef"

newJSRef :: JSCode -> JSCode
newJSRef expr =
  JSCode $ mconcat [fromString "JSRef.newJSRef((", unwrap expr, fromString "))"]

deRefJSRef :: JSRef -> JSCode
deRefJSRef (JSRef p) =
  JSCode $
  mconcat [fromString "JSRef.deRefJSRef(0x", hexadecimal p, fromString ")"]
