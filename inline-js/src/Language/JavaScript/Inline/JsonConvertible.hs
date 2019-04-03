{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.JavaScript.Inline.JsonConvertible
  ( JsonConvertible(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

class JsonConvertible a where
  stringify :: a -> Text
  parse :: LBS.ByteString -> a

instance (Aeson.ToJSON a, Aeson.FromJSON a) => JsonConvertible a where
  stringify = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
  parse = either error id . Aeson.eitherDecode
