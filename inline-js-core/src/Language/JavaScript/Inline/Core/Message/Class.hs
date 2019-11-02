module Language.JavaScript.Inline.Core.Message.Class
  ( Request (..),
    Response (..),
    encodeRequest,
    decodeResponse,
  )
where

import Control.Exception
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.MessageCounter

class Request r where
  putRequest :: r -> Put

class Response r where
  getResponse :: Get r

encodeRequest :: Request r => MsgId -> r -> LBS.ByteString
encodeRequest msg_id req = runPut $ do
  putWord32host $ fromIntegral msg_id
  putRequest req

decodeResponse :: Response r => LBS.ByteString -> IO r
decodeResponse buf = case runGetOrFail getResponse buf of
  Right (rest, _, resp) | LBS.null rest -> pure resp
  _ -> throwIO IllegalResponse {illegalResponse = buf}
