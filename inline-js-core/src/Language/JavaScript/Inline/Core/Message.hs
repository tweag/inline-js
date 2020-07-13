{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Message where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.String
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Utils

data JSExprSegment
  = Code LBS.ByteString
  | BufferLiteral LBS.ByteString
  | StringLiteral LBS.ByteString
  | JSONLiteral LBS.ByteString
  | JSValLiteral JSVal
  deriving (Show)

-- | Represents a JavaScript expression.
--
-- Use the 'IsString' instance to convert a 'String' to 'JSExpr', and the
-- 'Semigroup' instance for concating 'JSExpr'. It's also possible to embed
-- other things into 'JSExpr', e.g. a buffer literal, JSON value or a 'JSVal'.
newtype JSExpr = JSExpr
  { unJSExpr :: NE.NonEmpty JSExprSegment
  }
  deriving (Semigroup, Show)

instance IsString JSExpr where
  fromString = JSExpr . pure . Code . stringToLBS

-- | To convert a JavaScript value to Haskell, we need to specify its "raw
-- type", which can be one of the following:
data RawJSType
  = -- | The JavaScript value is discarded.
    RawNone
  | -- | The JavaScript value is an @ArrayBufferView@, @ArrayBuffer@ or
    -- @string@.
    RawBuffer
  | -- | The JavaScript value can be JSON-encoded via @JSON.stringify()@.
    RawJSON
  | -- | The JavaScript value should be managed as a 'JSVal'.
    RawJSVal
  deriving (Show)

data MessageHS
  = JSEvalRequest
      { jsEvalRequestId :: Word64,
        code :: JSExpr,
        returnType :: RawJSType
      }
  | HSExportRequest
      { exportRequestId :: Word64,
        exportFuncId :: Word64,
        argsType :: [(JSExpr, RawJSType)]
      }
  | HSEvalResponse
      { hsEvalResponseId :: Word64,
        hsEvalResponseContent :: Either LBS.ByteString JSExpr
      }
  | JSValFree Word64
  | Close
  deriving (Show)

data MessageJS
  = JSEvalResponse
      { jsEvalResponseId :: Word64,
        jsEvalResponseContent :: Either LBS.ByteString LBS.ByteString
      }
  | HSEvalRequest
      { hsEvalRequestId :: Word64,
        hsEvalRequestFunc :: Word64,
        args :: [LBS.ByteString]
      }
  | FatalError LBS.ByteString
  deriving (Show)

messageHSPut :: MessageHS -> Builder
messageHSPut msg = case msg of
  JSEvalRequest {..} ->
    word8Put 0
      <> word64Put jsEvalRequestId
      <> exprPut code
      <> rawTypePut returnType
  HSExportRequest {..} ->
    word8Put 1
      <> word64Put exportRequestId
      <> word64Put exportFuncId
      <> word64Put (fromIntegral (length argsType))
      <> foldMap'
        (\(code, raw_type) -> exprPut code <> rawTypePut raw_type)
        argsType
  HSEvalResponse {..} ->
    word8Put 2
      <> word64Put hsEvalResponseId
      <> ( case hsEvalResponseContent of
             Left err -> word8Put 0 <> lbsPut err
             Right r -> word8Put 1 <> exprPut r
         )
  JSValFree v -> word8Put 3 <> word64Put v
  Close -> word8Put 4
  where
    word8Put = storablePut @Word8
    word64Put = storablePut @Word64
    lbsPut s = storablePut (LBS.length s) <> lazyByteString s
    exprPut code =
      word64Put (fromIntegral (NE.length (unJSExpr code)) :: Word64)
        <> foldMap' exprSegmentPut (unJSExpr code)
    exprSegmentPut (Code s) = word8Put 0 <> lbsPut s
    exprSegmentPut (BufferLiteral s) = word8Put 1 <> lbsPut s
    exprSegmentPut (StringLiteral s) = word8Put 2 <> lbsPut s
    exprSegmentPut (JSONLiteral s) = word8Put 3 <> lbsPut s
    exprSegmentPut (JSValLiteral v) = word8Put 4 <> word64Put (unsafeUseJSVal v)
    rawTypePut RawNone = word8Put 0
    rawTypePut RawBuffer = word8Put 1
    rawTypePut RawJSON = word8Put 2
    rawTypePut RawJSVal = word8Put 3

messageJSGet :: Get MessageJS
messageJSGet = do
  t <- getWord8
  case t of
    0 -> do
      _id <- getWord64host
      _tag <- getWord8
      case _tag of
        0 -> do
          _err_buf <- lbsGet
          pure
            JSEvalResponse
              { jsEvalResponseId = _id,
                jsEvalResponseContent = Left _err_buf
              }
        1 -> do
          _result_buf <- lbsGet
          pure
            JSEvalResponse
              { jsEvalResponseId = _id,
                jsEvalResponseContent = Right _result_buf
              }
        _ -> fail $ "messageJSGet: invalid _tag " <> show _tag
    1 -> do
      _id <- getWord64host
      _func <- getWord64host
      l <- fromIntegral <$> getWord64host
      _args <- replicateM l lbsGet
      pure
        HSEvalRequest
          { hsEvalRequestId = _id,
            hsEvalRequestFunc = _func,
            args = _args
          }
    2 -> FatalError <$> lbsGet
    _ -> fail $ "messageJSGet: invalid tag " <> show t
  where
    lbsGet = do
      l <- fromIntegral <$> getWord64host
      getLazyByteString l
