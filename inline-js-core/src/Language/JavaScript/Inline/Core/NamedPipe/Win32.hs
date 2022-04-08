module Language.JavaScript.Inline.Core.NamedPipe.Win32 where

import Control.Monad
import Foreign
import Foreign.C
import GHC.IO.Handle.FD
import System.IO

mkNamedPipe :: Bool -> IO (String, IO Handle, IO ())
mkNamedPipe pipe_inbound = do
  (pipe_name, pipe_fd) <- allocaBytes 256 $ \buf_pipe_name ->
    alloca $ \buf_pipe_fd -> do
      r <- c_mkNamedPipe (fromBool pipe_inbound) buf_pipe_name 256 buf_pipe_fd
      unless (toBool r) $ fail "mkNamedPipe failed"
      pipe_name <- peekCString buf_pipe_name
      pipe_fd <- peek buf_pipe_fd
      pure (pipe_name, pipe_fd)
  h <- fdToHandle pipe_fd
  hSetBuffering h NoBuffering
  pure (pipe_name, pure h, pure ())

foreign import ccall unsafe "inline_js_mkNamedPipe"
  c_mkNamedPipe ::
    CBool -> CString -> CSize -> Ptr CInt -> IO CBool
