{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode(..)
  , codeToString
  , codeFromString
  , codeFromValueLBS
  , JSRef
  , parseJSRef
  , newJSRef
  , deRefJSRef
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text

newtype JSCode =
  JSCode Builder
  deriving (IsString)

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

newtype JSRef =
  JSRef Int
  deriving (Eq, Ord, Show)

parseJSRef :: LBS.ByteString -> Either String JSRef
parseJSRef buf =
  case Text.decimal $ Text.decodeUtf8 $ LBS.toStrict buf of
    Right (r, _) -> Right $ JSRef r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSRef"

newJSRef :: JSCode -> JSCode
newJSRef expr =
  JSCode $ mconcat [fromString "JSRef.newJSRef((", unwrap expr, fromString "))"]

deRefJSRef :: JSRef -> JSCode
deRefJSRef (JSRef p) =
  JSCode $ mconcat [fromString "JSRef.deRefJSRef(", intDec p, fromString ")"]
