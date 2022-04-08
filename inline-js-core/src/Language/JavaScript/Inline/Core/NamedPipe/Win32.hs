module Language.JavaScript.Inline.Core.NamedPipe.Win32 where

import Control.Monad
import Foreign
import Foreign.C
import System.IO
import System.Win32

mkNamedPipe :: Bool -> IO (String, IO Handle, IO ())
mkNamedPipe pipe_inbound = do
  (pipe_name, pipe_handle) <- allocaBytes 256 $ \buf_pipe_name ->
    alloca $ \buf_pipe_handle -> do
      r <-
        c_mkNamedPipe
          (fromBool pipe_inbound)
          buf_pipe_name
          256
          buf_pipe_handle
      unless (toBool r) $ fail "mkNamedPipe failed"
      pipe_name <- peekCString buf_pipe_name
      pipe_handle <- peek buf_pipe_handle
      pure (pipe_name, pipe_handle)
  h <- hANDLEToHandle pipe_handle
  pure (pipe_name, pure h, pure ())

foreign import ccall unsafe "inline_js_mkNamedPipe"
  c_mkNamedPipe ::
    CBool -> CString -> CSize -> Ptr HANDLE -> IO CBool
