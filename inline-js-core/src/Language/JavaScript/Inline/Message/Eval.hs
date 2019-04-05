{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message.Eval
  ( EvalRequest(..)
  , encodeEvalRequest
  , EvalResponse(..)
  , decodeEvalResponse
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Language.JavaScript.Inline.JSCode as JSCode
import Language.JavaScript.Inline.MessageCounter

data EvalRequest = EvalRequest
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Int
  , evalCode :: JSCode.JSCode
  }

encodeEvalRequest :: MsgId -> EvalRequest -> LBS.ByteString
encodeEvalRequest msg_id msg = runPut $ putEvalRequest msg_id msg

data EvalResponse = EvalResponse
  { isError :: Bool
  , result :: LBS.ByteString
  }

decodeEvalResponse :: LBS.ByteString -> Either String (MsgId, EvalResponse)
decodeEvalResponse buf =
  case runGetOrFail getEvalResponse buf of
    Left err -> Left $ show err
    Right (_, _, r) -> Right r

putEvalRequest :: MsgId -> EvalRequest -> Put
putEvalRequest msg_id EvalRequest {..} = do
  putWord32le $ fromIntegral msg_id
  putWord32le $
    if isAsync
      then 1
      else 0
  putWord32le $
    fromIntegral $
    case evalTimeout of
      Just t -> t
      _ -> 0
  putWord32le $
    fromIntegral $
    case resolveTimeout of
      Just t -> t
      _ -> 0
  putBuilder $ coerce evalCode

getEvalResponse :: Get (MsgId, EvalResponse)
getEvalResponse = do
  msg_id <- getWord32le
  is_err <- getWord32le
  r <- getRemainingLazyByteString
  pure (fromIntegral msg_id, EvalResponse {isError = is_err /= 0, result = r})
