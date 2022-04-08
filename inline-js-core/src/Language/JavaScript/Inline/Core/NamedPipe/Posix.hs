module Language.JavaScript.Inline.Core.NamedPipe.Posix where

import Control.Monad
import Foreign
import Foreign.C
import System.Directory
import System.IO

mkNamedPipe :: Bool -> IO (String, IO Handle, IO ())
mkNamedPipe pipe_inbound = do
  pipe_name <- allocaBytes 256 $ \buf_pipe_name -> do
    r <- c_mkNamedPipe buf_pipe_name 256
    unless (toBool r) $ fail "mkNamedPipe failed"
    peekCString buf_pipe_name
  pure
    ( pipe_name,
      do
        h <-
          openBinaryFile pipe_name $
            if pipe_inbound then ReadMode else WriteMode
        hSetBuffering h NoBuffering
        pure h,
      removePathForcibly pipe_name
    )

foreign import ccall unsafe "inline_js_mkNamedPipe"
  c_mkNamedPipe ::
    CString -> CSize -> IO CBool
