{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Class where

import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import Data.Proxy
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session

-- | UTF-8 encoded JSON.
newtype EncodedJSON = EncodedJSON
  { unEncodedJSON :: LBS.ByteString
  }
  deriving (Show)

-- | Haskell types which can be converted to JavaScript.
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

instance RawFromJS EncodedJSON where
  rawEval = coerce evalJSON

instance RawFromJS JSVal where
  rawEval = evalJSVal

-- | Haskell types which can be converted from JavaScript.
class
  (RawFromJS (RawJSType a)) =>
  FromJS a
  where
  -- | The raw JavaScript type. Must be one of:
  --
  -- 1. '()'. The JavaScript value is discarded.
  -- 2. 'LBS.ByteString'. The JavaScript value must be an
  --    @ArrayBufferView@(@Buffer@, @TypedArray@ or @DataView@), @ArrayBuffer@
  --    or @string@ (in which case it's UTF-8 encoded).
  -- 3. 'EncodedJSON'. The JavaScript value must be JSON-encodable via
  --    @JSON.stringify()@.
  -- 4. 'JSVal'. The JavaScript value can be of any type.
  type RawJSType a

  -- | A JavaScript function which encodes a value to the raw JavaScript type.
  toRawJSType :: Proxy a -> JSExpr

  -- | A Haskell function which decodes from the raw JavaScript type.
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
