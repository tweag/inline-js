{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Type
  ( Transport(..)
  ) where

import qualified Data.ByteString.Lazy as LBS

data Transport = Transport
  { closeTransport :: IO ()
  , sendData :: LBS.ByteString -> IO ()
  , recvData :: IO LBS.ByteString
  }
