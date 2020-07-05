{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Class where

import Control.Exception
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Language.JavaScript.Inline.Core
import System.IO.Unsafe

-- | If a Haskell type @a@ has 'A.ToJSON'/'A.FromJSON' instances, then @Aeson a@
-- has 'ToJS'/'FromJS' instances. We can generate 'ToJS'/'FromJS' instances for
-- type @a@ via:
--
-- 1. @deriving (ToJS, FromJS) via (Aeson a)@, using the @DerivingVia@ extension
-- 2. @deriving (ToJS, FromJS)@, using the @GeneralizedNewtypeDeriving@
--    extension
newtype Aeson a = Aeson
  { unAeson :: a
  }

-- | To embed a Haskell value into a 'JSExpr', its type should be an instance of
-- 'ToJS'.
class ToJS a where
  toJS :: a -> JSExpr

instance ToJS LBS.ByteString where
  toJS = buffer

instance A.ToJSON a => ToJS (Aeson a) where
  toJS = json . A.encode . unAeson

instance ToJS JSVal where
  toJS = jsval

class RawFromJS a where
  rawEval :: Session -> JSExpr -> IO a

instance RawFromJS () where
  rawEval = evalNone

instance RawFromJS LBS.ByteString where
  rawEval = evalBuffer

instance RawFromJS JSVal where
  rawEval = evalJSVal

-- | To decode a Haskell value from an eval result, its type should be an
-- instance of 'FromJS'.
class
  (RawFromJS (RawJSType a)) =>
  FromJS a
  where
  -- | The raw result type, must be one of '()', 'LBS.ByteString' or 'JSVal'.
  type RawJSType a

  -- | The JavaScript function which encodes a value to the raw result.
  toRawJSType :: Proxy a -> JSExpr

  -- | The Haskell function which decodes from the raw result.
  fromJS :: RawJSType a -> IO a

instance FromJS () where
  type RawJSType () = ()
  toRawJSType _ = "a => a"
  fromJS = pure

instance FromJS LBS.ByteString where
  type RawJSType LBS.ByteString = LBS.ByteString
  toRawJSType _ = "a => a"
  fromJS = pure

instance A.FromJSON a => FromJS (Aeson a) where
  type RawJSType (Aeson a) = LBS.ByteString
  toRawJSType _ = "a => Buffer.from(JSON.stringify(a))"
  fromJS s = case A.eitherDecode' s of
    Left err -> fail err
    Right a -> pure $ Aeson a

instance FromJS JSVal where
  type RawJSType JSVal = JSVal
  toRawJSType _ = "a => a"
  fromJS = pure

-- | The polymorphic eval function. Similar to the eval functions in
-- "Language.JavaScript.Inline.Core", 'eval' performs /asynchronous/ evaluation
-- and returns a thunk. Forcing the thunk will block until the result is
-- returned from @node@ and decoded.
eval :: forall a. FromJS a => Session -> JSExpr -> IO a
eval s c = do
  r <-
    rawEval s $
      "Promise.resolve("
        <> c
        <> ").then("
        <> toRawJSType (Proxy @a)
        <> ")"
  unsafeInterleaveIO $ fromJS =<< evaluate r
