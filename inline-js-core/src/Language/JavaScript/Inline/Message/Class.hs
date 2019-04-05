{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.JavaScript.Inline.Message.Class
  ( Message(..)
  , encodeRequest
  , decodeResponse
  ) where

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.MessageCounter

class Message i o | i -> o, o -> i where
  putRequest :: i -> Put
  getResponse :: Get o

encodeRequest :: Message i o => MsgId -> i -> LBS.ByteString
encodeRequest msg_id req =
  runPut $ do
    putWord32host $ fromIntegral msg_id
    putRequest req

decodeResponse :: Message i o => MsgId -> LBS.ByteString -> IO o
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
