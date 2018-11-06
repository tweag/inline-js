{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Message
  ( SendMsg(..)
  , encodeSendMsg
  , RecvMsg(..)
  , decodeRecvMsg
  ) where

import qualified Language.JavaScript.Inline.JSCode as JSCode
import qualified Language.JavaScript.Inline.JSON as JSON
import Language.JavaScript.Inline.MessageCounter

data SendMsg = Eval
  { evalCode :: JSCode.JSCode
  , evalTimeout, resolveTimeout :: Maybe Double
  , isAsync :: Bool
  , region :: Maybe JSCode.JSRefRegion
  } deriving (Show)

encodeSendMsg :: MsgId -> SendMsg -> JSON.Value
encodeSendMsg msg_id msg =
  case msg of
    Eval {..} ->
      JSON.Array
        [ _head
        , JSON.Number 0
        , JSON.String $ JSCode.codeToString evalCode
        , maybe (JSON.Bool False) JSON.Number evalTimeout
        , maybe (JSON.Bool False) JSON.Number resolveTimeout
        , JSON.Bool isAsync
        , maybe (JSON.Bool False) JSCode.valueFromJSRefRegion region
        ]
  where
    _head = JSON.Number $ fromIntegral msg_id

data RecvMsg = Result
  { isError :: Bool
  , result :: JSON.Value
  } deriving (Show)

decodeRecvMsg :: JSON.Value -> Either String (MsgId, RecvMsg)
decodeRecvMsg v =
  case v of
    JSON.Array [JSON.Number _msg_id, JSON.Number 0, JSON.Bool is_err, r] ->
      Right (truncate _msg_id, Result {isError = is_err, result = r})
    _ -> _err
  where
    _err =
      Left $
      "Language.JavaScript.Inline.Message.decodeRecvMsg: failed to decode " <>
      show v
