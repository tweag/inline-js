{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Transport.Process
  ( ProcessTransportOpts(..)
  , newProcessTransport
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString.Builder
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Word
import Foreign
import GHC.IO.Handle.FD
import Language.JavaScript.Inline.Transport.Type
import qualified Paths_inline_js_core
import System.Directory
import System.FilePath
import System.IO
import System.Process

data ProcessTransportOpts = ProcessTransportOpts
  { nodePath :: FilePath
  , nodeExtraArgs :: [String]
  , nodeWorkDir :: Maybe FilePath
  , nodeStdInInherit, nodeStdOutInherit, nodeStdErrInherit :: Bool
  }

newProcessTransport ::
     ProcessTransportOpts
  -> IO (Transport, Maybe Handle, Maybe Handle, Maybe Handle)
newProcessTransport ProcessTransportOpts {..} = do
  (mjss_dir, mjss) <-
    do mjss_dir <- (</> "jsbits") <$> Paths_inline_js_core.getDataDir
       case nodeWorkDir of
         Just p -> do
           mjss <- listDirectory mjss_dir
           for_ mjss $ \mjs -> copyFile (mjss_dir </> mjs) (p </> mjs)
           pure (p, mjss)
         _ -> pure (mjss_dir, [])
  (rh0, wh0) <- createPipe
  (rh1, wh1) <- createPipe
  for_ [rh0, wh0, rh1, wh1] $ \h -> do
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
  wfd0 <- handleToFd wh0
  rfd1 <- handleToFd rh1
  (_m_stdin, _m_stdout, _m_stderr, _ph) <-
    createProcess
      (proc nodePath $
       nodeExtraArgs <>
       ["--experimental-modules", mjss_dir </> "eval.mjs", show wfd0, show rfd1])
        { cwd = nodeWorkDir
        , std_in =
            if nodeStdInInherit
              then Inherit
              else CreatePipe
        , std_out =
            if nodeStdOutInherit
              then Inherit
              else CreatePipe
        , std_err =
            if nodeStdErrInherit
              then Inherit
              else CreatePipe
        }
  pure
    ( Transport
        { closeTransport =
            do terminateProcess _ph
               case nodeWorkDir of
                 Just p -> for_ mjss $ \mjs -> removeFile $ p </> mjs
                 _ -> pure ()
        , sendData =
            \buf -> do
              buf' <- evaluate $ force buf
              hPutBuilder wh1 $
                word32LE (fromIntegral $ LBS.length buf') <> lazyByteString buf'
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
