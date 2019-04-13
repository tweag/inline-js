{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Type
  ( Transport(..)
  ) where

import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.MessageCounter

data Transport = Transport
  { closeTransport :: IO ()
  , sendData :: LBS.ByteString -> IO ()
  , recvData :: MsgId -> IO LBS.ByteString
  }
