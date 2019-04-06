module Language.JavaScript.Inline.Message.Class
  ( Request(..)
  , Response(..)
  , encodeRequest
  , decodeResponse
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.MessageCounter

class Request r where
  putRequest :: r -> Put

class Response r where
  getResponse :: Get r

encodeRequest :: Request r => MsgId -> r -> LBS.ByteString
encodeRequest msg_id req =
  runPut $ do
    putWord32host $ fromIntegral msg_id
    putRequest req

decodeResponse :: Response r => MsgId -> LBS.ByteString -> IO r
decodeResponse msg_id buf =
  case runGetOrFail
         ((,) <$> (fromIntegral <$> getWord32host) <*> getResponse)
         buf of
    Right (rest, _, (msg_id', resp))
      | LBS.null rest && msg_id == msg_id' -> pure resp
    _ ->
      fail $
      "Language.JavaScript.Inline.Message.Class.decodeResponse: failed to decode message " <>
      show msg_id <>
      " from " <>
      show buf
