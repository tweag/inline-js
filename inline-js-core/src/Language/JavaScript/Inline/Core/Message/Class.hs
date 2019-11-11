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
import Foreign.Ptr
import Foreign.StablePtr
import Language.JavaScript.Inline.Core.Exception

class Request r where
  putRequest :: r -> Put

class Response r where
  getResponse :: Get r

encodeRequest :: Request r => StablePtr a -> r -> LBS.ByteString
encodeRequest msg_id req = runPut $ do
  putWord32host $ fromIntegral $ ptrToIntPtr $ castStablePtrToPtr msg_id
  putRequest req

decodeResponse :: Response r => LBS.ByteString -> IO r
decodeResponse buf = case runGetOrFail getResponse buf of
  Right (rest, _, resp) | LBS.null rest -> pure resp
  _ -> throwIO IllegalResponse {illegalResponse = buf}
