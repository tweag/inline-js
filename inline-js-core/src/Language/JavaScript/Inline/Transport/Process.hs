{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Process
  ( ProcessTransportOpts(..)
  , newProcessTransport
  ) where

import Data.ByteString.Builder
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.ForeignPtr
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Transport.Type
import System.IO
import System.Process

data ProcessTransportOpts = ProcessTransportOpts
  { procPath :: FilePath
  , procArgs :: [String]
  , procStdOutInherit, procStdErrInherit :: Bool
  }

newProcessTransport :: ProcessTransportOpts -> IO Transport
newProcessTransport ProcessTransportOpts {..} = do
  (rh0, wh0) <- createPipe
  (rh1, wh1) <- createPipe
  for_ [rh0, wh0, rh1, wh1] $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
  wfd0 <- handleToFd wh0
  rfd1 <- handleToFd rh1
  (_, _, _, _ph) <-
    createProcess
      (proc procPath $ procArgs <> [show wfd0, show rfd1])
        { std_in = CreatePipe
        , std_out =
            if procStdOutInherit
              then Inherit
              else CreatePipe
        , std_err =
            if procStdErrInherit
              then Inherit
              else CreatePipe
        }
  pure
    Transport
      { closeTransport = terminateProcess _ph
      , sendData =
          \buf ->
            hPutBuilder wh1 $
            word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
      , recvData =
          do lp <- hGetForeignPtr rh0 4
             len <- withForeignPtr lp peek
             bp <- hGetForeignPtr rh0 $ fromIntegral (len :: Word32)
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
