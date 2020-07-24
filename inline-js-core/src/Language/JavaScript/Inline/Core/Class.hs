{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Class where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.String
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils

-- | UTF-8 encoded string.
newtype EncodedString = EncodedString
  { unEncodedString :: LBS.ByteString
  }
  deriving (Show, IsString)

-- | UTF-8 encoded JSON.
newtype EncodedJSON = EncodedJSON
  { unEncodedJSON :: LBS.ByteString
  }
  deriving (Show, IsString)

-- | Haskell types which can be converted to JavaScript.
class ToJS a where
  -- | Encodes a Haskell value to 'JSExpr'.
  toJS :: a -> JSExpr

instance ToJS () where
  toJS _ = "undefined"

instance ToJS LBS.ByteString where
  toJS = JSExpr . pure . BufferLiteral

instance ToJS EncodedString where
  toJS = JSExpr . pure . StringLiteral . unEncodedString

instance ToJS EncodedJSON where
  toJS = JSExpr . pure . JSONLiteral . unEncodedJSON

instance ToJS JSVal where
  toJS = JSExpr . pure . JSValLiteral

-- | Haskell types which can be converted from JavaScript.
class FromJS a where
  -- | The JavaScript value's 'RawJSType'.
  rawJSType :: Proxy a -> RawJSType

  -- | A synchronous JavaScript function which encodes a value to its
  -- 'RawJSType'.
  toRawJSType :: Proxy a -> JSExpr

  -- | A Haskell function which decodes the Haskell value from the serialized
  -- 'RawJSType'.
  fromJS :: Session -> LBS.ByteString -> IO a

instance FromJS () where
  rawJSType _ = RawNone
  toRawJSType _ = "() => undefined"
  fromJS _ _ = pure ()

instance FromJS LBS.ByteString where
  rawJSType _ = RawBuffer
  toRawJSType _ = "a => a"
  fromJS _ = pure

instance FromJS EncodedString where
  rawJSType _ = RawBuffer
  toRawJSType _ = "a => a"
  fromJS _ = pure . EncodedString

instance FromJS EncodedJSON where
  rawJSType _ = RawJSON
  toRawJSType _ = "a => a"
  fromJS _ = pure . EncodedJSON

instance FromJS JSVal where
  rawJSType _ = RawJSVal
  toRawJSType _ = "a => a"
  fromJS _session _jsval_id_buf = do
    _jsval_id <- runGetExact getWord64host _jsval_id_buf
    newJSVal True _jsval_id (sessionSend _session $ JSValFree _jsval_id)
