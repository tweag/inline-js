{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Class where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Proxy
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import System.IO.Unsafe

-- | To embed a Haskell value into a 'JSExpr', its type should be an instance of
-- 'ToJS'.
class ToJS a where
  toJS :: a -> JSExpr

instance ToJS LBS.ByteString where
  toJS = JSExpr . pure . BufferLiteral

instance ToJS EncodedJSON where
  toJS = JSExpr . pure . BufferLiteral . unEncodedJSON

instance ToJS JSVal where
  toJS = JSExpr . pure . JSValLiteral

class RawFromJS a where
  rawEval :: Session -> JSExpr -> IO a

instance RawFromJS () where
  rawEval = evalNone

instance RawFromJS LBS.ByteString where
  rawEval = evalBuffer

-- | UTF-8 encoded JSON.
newtype EncodedJSON = EncodedJSON
  { unEncodedJSON :: LBS.ByteString
  }

instance RawFromJS EncodedJSON where
  rawEval = coerce evalJSON

instance RawFromJS JSVal where
  rawEval = evalJSVal

-- | To decode a Haskell value from an eval result, its type should be an
-- instance of 'FromJS'.
class
  (RawFromJS (RawJSType a)) =>
  FromJS a
  where
  -- | The raw JavaScript type. Must be one of:
  --
  -- 1. '()'. The JavaScript eval result is ignored.
  -- 2. 'LBS.ByteString'. The JavaScript eval result must be an
  --    @ArrayBufferView@(@Buffer@, @TypedArray@ or @DataView@) or
  --    @ArrayBuffer@.
  -- 3. 'EncodedJSON'. The JavaScript eval result must be JSON-encodable via
  --    @JSON.stringify()@.
  -- 4. 'JSVal'. The JavaScript eval result can be of any type.
  type RawJSType a

  -- | The JavaScript function which encodes a value to the raw JavaScript type.
  toRawJSType :: Proxy a -> JSExpr

  -- | The Haskell function which decodes from the raw JavaScript type.
  fromRawJSType :: RawJSType a -> IO a

instance FromJS () where
  type RawJSType () = ()
  toRawJSType _ = "a => a"
  fromRawJSType = pure

instance FromJS LBS.ByteString where
  type RawJSType LBS.ByteString = LBS.ByteString
  toRawJSType _ = "a => a"
  fromRawJSType = pure

instance FromJS EncodedJSON where
  type RawJSType EncodedJSON = EncodedJSON
  toRawJSType _ = "a => a"
  fromRawJSType = pure

instance FromJS JSVal where
  type RawJSType JSVal = JSVal
  toRawJSType _ = "a => a"
  fromRawJSType = pure

-- | The polymorphic eval function.
eval :: forall a. FromJS a => Session -> JSExpr -> IO a
eval s c = do
  r <-
    rawEval s $
      "Promise.resolve("
        <> c
        <> ").then("
        <> toRawJSType (Proxy @a)
        <> ")"
  unsafeInterleaveIO $ fromRawJSType =<< evaluate r
