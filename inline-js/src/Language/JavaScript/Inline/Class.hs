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
-- has 'ToJSCode'/'FromEvalResult' instances, and we can use @deriving
-- (ToJSCode, FromEvalResult) via (Aeson a)@ to generate
-- 'ToJSCode'/'FromEvalResult' instances for that type.
newtype Aeson a = Aeson
  { unAeson :: a
  }

-- | To embed a Haskell value into a 'JSCode', its type should be an instance of
-- 'ToJSCode'.
class ToJSCode a where
  toJSCode :: a -> JSCode

instance ToJSCode LBS.ByteString where
  toJSCode = buffer

instance A.ToJSON a => ToJSCode (Aeson a) where
  toJSCode = json . A.encode . unAeson

instance ToJSCode JSVal where
  toJSCode = jsval

class RawEval a where
  rawEval :: Session -> JSCode -> IO a

instance RawEval () where
  rawEval = evalNone

instance RawEval LBS.ByteString where
  rawEval = evalBuffer

instance RawEval JSVal where
  rawEval = evalJSVal

-- | To decode a Haskell value from an eval result, its type should be an
-- instance of 'FromEvalResult'.
class
  (RawEval (EvalResult a)) =>
  FromEvalResult a
  where
  -- | The raw result type, must be one of '()', 'LBS.ByteString' or 'JSVal'.
  type EvalResult a

  -- | The JavaScript function which encodes a value to the raw result.
  toEvalResult :: Proxy a -> JSCode

  -- | The Haskell function which decodes from the raw result.
  fromEvalResult :: EvalResult a -> IO a

instance FromEvalResult () where
  type EvalResult () = ()
  toEvalResult _ = "a => a"
  fromEvalResult = pure

instance FromEvalResult LBS.ByteString where
  type EvalResult LBS.ByteString = LBS.ByteString
  toEvalResult _ = "a => a"
  fromEvalResult = pure

instance A.FromJSON a => FromEvalResult (Aeson a) where
  type EvalResult (Aeson a) = LBS.ByteString
  toEvalResult _ = "a => Buffer.from(JSON.stringify(a))"
  fromEvalResult s = case A.eitherDecode' s of
    Left err -> fail err
    Right a -> pure $ Aeson a

instance FromEvalResult JSVal where
  type EvalResult JSVal = JSVal
  toEvalResult _ = "a => a"
  fromEvalResult = pure

-- | The polymorphic eval function.
eval :: forall a. FromEvalResult a => Session -> JSCode -> IO a
eval s c = do
  r <-
    rawEval s $
      "Promise.resolve("
        <> c
        <> ").then("
        <> toEvalResult (Proxy @a)
        <> ")"
  unsafeInterleaveIO $ fromEvalResult =<< evaluate r
