{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Message.Class
  ( Request(..)
  , Response(..)
  , encodeRequest
  , decodeResponse
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core.MessageCounter

-- | The class of supported request types.
class Request r where
  type ResponseOf r
  putRequest :: r -> Put

-- | The class of supported response types.
class Response r where
  getResponse :: Get r

encodeRequest :: Request r => MsgId -> r -> LBS.ByteString
encodeRequest msg_id req =
  runPut $ do
    putWord32host $ fromIntegral msg_id
    putRequest req

decodeResponse :: Response r => LBS.ByteString -> IO r
decodeResponse buf =
  case runGetOrFail getResponse buf of
    Right (rest, _, resp)
      | LBS.null rest -> pure resp
    _ ->
      fail $
      "Language.JavaScript.Inline.Core.Message.Class.decodeResponse: failed to decode message from " <>
      show buf
