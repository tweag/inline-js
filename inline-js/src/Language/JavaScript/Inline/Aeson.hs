{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Aeson where

import qualified Data.Aeson as A
import Language.JavaScript.Inline.Core

-- | If a Haskell type @a@ has 'A.ToJSON' and 'A.FromJSON' instances, then we
-- can derive 'ToJS' and 'FromJS' instances for it using:
--
-- 1. @deriving (ToJS, FromJS) via (Aeson a)@, using the @DerivingVia@ extension
-- 2. @deriving (ToJS, FromJS)@, using the @GeneralizedNewtypeDeriving@
--    extension
newtype Aeson a = Aeson
  { unAeson :: a
  }

instance A.ToJSON a => ToJS (Aeson a) where
  toJS = toJS . EncodedJSON . A.encode . unAeson

instance A.FromJSON a => FromJS (Aeson a) where
  type RawJSType (Aeson a) = EncodedJSON
  toRawJSType _ = "a => a"
  fromRawJSType s = case A.eitherDecode' (unEncodedJSON s) of
    Left err -> fail err
    Right a -> pure $ Aeson a
