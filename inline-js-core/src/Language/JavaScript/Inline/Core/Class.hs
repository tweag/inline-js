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

-- | Haskell types which can be converted from JavaScript.
class FromJS a where
  rawJSType :: Proxy a -> RawJSType

  -- | A JavaScript function which encodes a value to the raw JavaScript type.
  toRawJSType :: Proxy a -> JSExpr

  fromJS :: Session -> LBS.ByteString -> IO a

instance FromJS () where
  rawJSType _ = RawNone
  toRawJSType _ = "a => a"
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
    newJSVal _jsval_id (sessionSend _session $ JSValFree _jsval_id)
