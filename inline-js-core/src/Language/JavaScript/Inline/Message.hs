{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message
  ( SendMsg(..)
  , encodeSendMsg
  , RecvMsg(..)
  , decodeRecvMsg
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce
import qualified Language.JavaScript.Inline.JSCode as JSCode
import Language.JavaScript.Inline.MessageCounter

data SendMsg = Eval
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Double
  , evalCode :: JSCode.JSCode
  }

encodeSendMsg :: MsgId -> SendMsg -> LBS.ByteString
encodeSendMsg msg_id msg = runPut $ putSendMsg msg_id msg

data RecvMsg = Result
  { isError :: Bool
  , result :: LBS.ByteString
  }

decodeRecvMsg :: LBS.ByteString -> Either String (MsgId, RecvMsg)
decodeRecvMsg buf =
  case runGetOrFail getRecvMsg buf of
    Left err -> Left $ show err
    Right (_, _, r) -> Right r

putSendMsg :: MsgId -> SendMsg -> Put
putSendMsg msg_id Eval {..} = do
  putWord32le $ fromIntegral msg_id
  putWord32le $
    if isAsync
      then 1
      else 0
  putDoublele $
    case evalTimeout of
      Just t -> t
      _ -> 1 / 0
  putDoublele $
    case resolveTimeout of
      Just t -> t
      _ -> 1 / 0
  putBuilder $ coerce evalCode

getRecvMsg :: Get (MsgId, RecvMsg)
getRecvMsg = do
  msg_id <- getWord32le
  is_err <- getWord32le
  r <- getRemainingLazyByteString
  pure (fromIntegral msg_id, Result {isError = is_err /= 0, result = r})
