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

data SendMsg
  = Ping JSON.Value
  | Eval { evalCode :: JSCode.JSCode
         , evalTimeout :: Maybe Double }
  | EvalAsync { evalAsyncCode :: JSCode.JSCode
              , evalAsyncTimeout, resolveAsyncTimeout :: Maybe Double }
  deriving (Show)

encodeSendMsg :: MsgId -> SendMsg -> JSON.Value
encodeSendMsg msg_id msg =
  case msg of
    Ping v -> JSON.Array [_head, JSON.Number 0, v]
    Eval {..} ->
      JSON.Array
        [ _head
        , JSON.Number 1
        , JSON.Array
            [ JSON.String $ JSCode.codeToString evalCode
            , _maybe_number evalTimeout
            ]
        ]
    EvalAsync {..} ->
      JSON.Array
        [ _head
        , JSON.Number 2
        , JSON.Array
            [ JSON.String $ JSCode.codeToString evalAsyncCode
            , _maybe_number evalAsyncTimeout
            , _maybe_number resolveAsyncTimeout
            ]
        ]
  where
    _head = JSON.Number $ fromIntegral msg_id
    _maybe_number = maybe (JSON.Bool False) JSON.Number

data RecvMsg
  = Error JSON.Value
  | Ready
  | Pong JSON.Value
  | Result JSON.Value
  deriving (Show)

decodeRecvMsg :: JSON.Value -> Either String (MsgId, RecvMsg)
decodeRecvMsg v =
  case v of
    JSON.Array [JSON.Number _msg_id, JSON.Number 0, err] ->
      Right (truncate _msg_id, Error err)
    JSON.Array [JSON.Number 0, JSON.Number 1, JSON.Null] -> Right (0, Ready)
    JSON.Array [JSON.Number _msg_id, JSON.Number 2, r] ->
      Right (truncate _msg_id, Pong r)
    JSON.Array [JSON.Number _msg_id, JSON.Number 3, r] ->
      Right (truncate _msg_id, Result r)
    _ -> _err
  where
    _err =
      Left $
      "Language.JavaScript.Inline.Message.decodeRecvMsg: failed to decode " <>
      show v
