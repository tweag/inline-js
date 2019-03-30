{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Process
  ( ProcessTransportOpts(..)
  , newProcessTransport
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Language.JavaScript.Inline.Transport.Type
import System.IO
import System.Process

data ProcessTransportOpts = ProcessTransportOpts
  { procPath :: FilePath
  , procArgs :: [String]
  , procStdErrInherit :: Bool
  }

newProcessTransport :: ProcessTransportOpts -> IO Transport
newProcessTransport ProcessTransportOpts {..} = do
  (Just _stdin, Just _stdout, _m_stderr, _ph) <-
    createProcess
      (proc procPath procArgs)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err =
            if procStdErrInherit
              then Inherit
              else CreatePipe
        }
  hSetBinaryMode _stdin True
  hSetBuffering _stdin $ BlockBuffering Nothing
  hSetBinaryMode _stdout True
  hSetBuffering _stdout $ BlockBuffering Nothing
  pure
    Transport
      { closeTransport = terminateProcess _ph
      , sendData =
          \buf -> do
            hPutBuilder _stdin $
              word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
            hFlush _stdin
      , recvData =
          do lbuf <- BS.hGet _stdout 4
             len <-
               BS.unsafeUseAsCStringLen lbuf $ \(lptr, _) ->
                 fromIntegral <$> peek (castPtr lptr :: Ptr Word32)
             LBS.hGet _stdout len
      }
