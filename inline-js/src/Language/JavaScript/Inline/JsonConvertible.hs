{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.JavaScript.Inline.JsonConvertible
  ( JsonConvertible(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

class JsonConvertible a where
  stringify :: a -> Text
  parse :: Text -> a

instance (Aeson.ToJSON a, Aeson.FromJSON a) => JsonConvertible a where
  stringify = Text.decodeUtf8 . ByteString.toStrict . Aeson.encode
  parse =
    either error id .
    Aeson.eitherDecode . ByteString.fromStrict . Text.encodeUtf8
