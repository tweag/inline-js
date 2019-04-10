{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Process
  ( ProcessTransportOpts(..)
  , newProcessTransport
  ) where

import Control.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Word
import Foreign
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Transport.Type
import System.IO
import System.Process

data ProcessTransportOpts = ProcessTransportOpts
  { procPath :: FilePath
  , procArgs :: [String]
  , procWorkDir :: Maybe FilePath
  , procStdInInherit, procStdOutInherit, procStdErrInherit :: Bool
  }

newProcessTransport ::
     ProcessTransportOpts
  -> IO (Transport, Maybe Handle, Maybe Handle, Maybe Handle)
newProcessTransport ProcessTransportOpts {..} = do
  (rh0, wh0) <- createPipe
  (rh1, wh1) <- createPipe
  for_ [rh0, wh0, rh1, wh1] $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
  wfd0 <- handleToFd wh0
  rfd1 <- handleToFd rh1
  (_m_stdin, _m_stdout, _m_stderr, _ph) <-
    createProcess
      (proc procPath $ procArgs <> [show wfd0, show rfd1])
        { cwd = procWorkDir
        , std_in =
            if procStdInInherit
              then Inherit
              else CreatePipe
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
    ( Transport
        { closeTransport = terminateProcess _ph
        , sendData =
            \buf ->
              hPutBuilder wh1 $
              word32LE (fromIntegral $ LBS.length buf) <> lazyByteString buf
        , recvData =
            do len <-
                 alloca $ \p -> do
                   hGet' rh0 p 4
                   peek p
               let len' = fromIntegral (len :: Word32)
               fmap LBS.fromStrict $ BS.create len' $ \p -> hGet' rh0 p len'
        }
    , _m_stdin
    , _m_stdout
    , _m_stderr)

hGet' :: Handle -> Ptr a -> Int -> IO ()
hGet' h p l = do
  l' <- hGetBuf h p l
  unless (l' == l) $
    fail $ "hGet': expected " <> show l <> " bytes, got " <> show l'
