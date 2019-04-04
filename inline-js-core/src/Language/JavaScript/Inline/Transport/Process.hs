{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Process
  ( ProcessTransportOpts(..)
  , newProcessTransport
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr
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
          do lp <- hGetForeignPtr _stdout 4
             len <- withForeignPtr lp peek
             bp <- hGetForeignPtr _stdout $ fromIntegral (len :: Word32)
             pure $ LBS.fromStrict $ BS.fromForeignPtr bp 0 $ fromIntegral len
      }

hGetForeignPtr :: Handle -> Int -> IO (ForeignPtr a)
hGetForeignPtr h l = do
  fp <- mallocPlainForeignPtrBytes l
  withForeignPtr fp $ \p -> do
    l' <- hGetBuf h p l
    if l' == l
      then pure fp
      else fail $
           "hGetForeignPtr: expected " <> show l <> " bytes, got " <> show l'
