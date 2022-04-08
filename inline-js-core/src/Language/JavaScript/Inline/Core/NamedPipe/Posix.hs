module Language.JavaScript.Inline.Core.NamedPipe.Posix where

import Control.Monad
import Foreign
import Foreign.C
import GHC.IO.Handle.FD (fdToHandle)
import System.Directory
import System.IO
import System.Posix.Internals

mkNamedPipe :: Bool -> IO (String, IO Handle, IO ())
mkNamedPipe pipe_inbound = do
  pipe_name <- allocaBytes 256 $ \buf_pipe_name -> do
    r <- c_mkNamedPipe buf_pipe_name 256
    unless (toBool r) $ fail "mkNamedPipe failed"
    peekCString buf_pipe_name
  pure
    ( pipe_name,
      do
        pipe_fd <- withFilePath pipe_name $ \buf_pipe_name ->
          c_safe_open
            buf_pipe_name
            (if pipe_inbound then o_RDONLY else o_WRONLY)
            0o600
        h <- fdToHandle pipe_fd
        hSetBuffering h NoBuffering
        pure h,
      removePathForcibly pipe_name
    )

foreign import ccall unsafe "inline_js_mkNamedPipe"
  c_mkNamedPipe ::
    CString -> CSize -> IO CBool
