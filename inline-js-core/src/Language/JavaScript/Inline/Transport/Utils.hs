module Language.JavaScript.Inline.Transport.Utils
  ( lockTransport
  , strictTransport
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import Control.Exception
import Language.JavaScript.Inline.Transport.Type

lockTransport :: Transport -> IO Transport
lockTransport t = do
  _send_lock <- newMVar ()
  _recv_lock <- newMVar ()
  pure
    t
      { sendData = \buf -> withMVar _send_lock $ \_ -> sendData t buf
      , recvData = withMVar _recv_lock $ \_ -> recvData t
      }

strictTransport :: Transport -> Transport
strictTransport t =
  t
    { sendData =
        \buf' -> do
          buf <- evaluate $ force buf'
          sendData t buf
    , recvData =
        do buf' <- recvData t
           evaluate $ force buf'
    }
