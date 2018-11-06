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
