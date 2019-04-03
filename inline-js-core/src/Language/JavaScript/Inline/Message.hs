{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message
  ( SendMsg(..)
  , encodeSendMsg
  , RecvMsg(..)
  , decodeRecvMsg
  ) where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Language.JavaScript.Inline.JSCode as JSCode
import qualified Language.JavaScript.Inline.JSON as JSON
import Language.JavaScript.Inline.MessageCounter

data SendMsg = Eval
  { isAsync :: Bool
  , evalTimeout, resolveTimeout :: Maybe Double
  , evalCode :: JSCode.JSCode
  }

encodeSendMsg :: MsgId -> SendMsg -> JSON.Value
encodeSendMsg msg_id msg =
  case msg of
    Eval {..} ->
      JSON.Array
        [ _head
        , JSON.Bool isAsync
        , _maybe_number evalTimeout
        , _maybe_number resolveTimeout
        , JSON.String $ JSCode.codeToString evalCode
        ]
  where
    _head = JSON.Number $ fromIntegral msg_id
    _maybe_number = maybe (JSON.Bool False) JSON.Number

data RecvMsg = Result
  { isError :: Bool
  , result :: LBS.ByteString
  }

decodeRecvMsg :: LBS.ByteString -> Either String (MsgId, RecvMsg)
decodeRecvMsg buf =
  case runGetOrFail getRecvMsg buf of
    Left err -> Left $ show err
    Right (_, _, r) -> Right r

getRecvMsg :: Get (MsgId, RecvMsg)
getRecvMsg = do
  msg_id <- getWord32le
  is_err <- getWord32le
  r <- getRemainingLazyByteString
  pure (fromIntegral msg_id, Result {isError = is_err /= 0, result = r})
