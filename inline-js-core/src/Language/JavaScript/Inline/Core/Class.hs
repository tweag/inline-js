{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Class where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils

-- | UTF-8 encoded string.
newtype EncodedString = EncodedString
  { unEncodedString :: LBS.ByteString
  }
  deriving (Show)

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

instance ToJS EncodedString where
  toJS = JSExpr . pure . StringLiteral . unEncodedString

instance ToJS EncodedJSON where
  toJS = JSExpr . pure . BufferLiteral . unEncodedJSON

instance ToJS JSVal where
  toJS = JSExpr . pure . JSValLiteral

class RawFromJS a where
  rawType :: Proxy a -> JSRawType
  rawFromJS :: Session -> LBS.ByteString -> IO a

instance RawFromJS () where
  rawType _ = RawNone
  rawFromJS _ _ = pure ()

instance RawFromJS LBS.ByteString where
  rawType _ = RawBuffer
  rawFromJS _ buf = pure buf

instance RawFromJS EncodedString where
  rawType _ = RawBuffer
  rawFromJS _ buf = pure $ EncodedString buf

instance RawFromJS EncodedJSON where
  rawType _ = RawJSON
  rawFromJS _ buf = pure $ EncodedJSON buf

instance RawFromJS JSVal where
  rawType _ = RawJSVal
  rawFromJS _session _jsval_id_buf = do
    _jsval_id <- runGetExact getWord64host _jsval_id_buf
    newJSVal _jsval_id (sessionSend _session $ JSValFree _jsval_id)

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

instance FromJS EncodedString where
  type RawJSType EncodedString = EncodedString
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
