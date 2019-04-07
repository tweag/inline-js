{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.JavaScript.Inline.JSCode
  ( JSCode(..)
  , codeToString
  , codeFromString
  , codeFromValueLBS
  , JSVal(..)
  , parseJSVal
  , newJSVal
  , deRefJSVal
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

newtype JSVal =
  JSVal Int
  deriving (Eq, Ord, Show)

parseJSVal :: LBS.ByteString -> Either String JSVal
parseJSVal buf =
  case Text.decimal $ Text.decodeUtf8 $ LBS.toStrict buf of
    Right (r, _) -> Right $ JSVal r
    _ -> Left "Language.JavaScript.Inline.JSCode.parseJSVal"

newJSVal :: JSCode -> JSCode
newJSVal expr =
  JSCode $ mconcat [fromString "JSVal.newJSVal((", unwrap expr, fromString "))"]

deRefJSVal :: JSVal -> JSCode
deRefJSVal (JSVal p) =
  JSCode $ mconcat [fromString "JSVal.deRefJSVal(", intDec p, fromString ")"]
