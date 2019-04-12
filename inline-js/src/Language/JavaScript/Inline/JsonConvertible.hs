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

instance Aeson.ToJSON a => JsonConvertible a where
  stringify = Text.decodeUtf8 . LBS.toStrict . Aeson.encode
