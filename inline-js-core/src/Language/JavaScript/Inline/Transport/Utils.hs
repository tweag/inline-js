module Language.JavaScript.Inline.Transport.Utils
  ( lockSend
  , strictTransport
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Data.Functor
import Language.JavaScript.Inline.Transport.Type

lockSend :: Transport -> IO Transport
lockSend t = do
  q <- newTQueueIO
  void $
    forkIO $
    let w = do
          mbuf <- atomically $ readTQueue q
          case mbuf of
            Just buf -> do
              sendData t buf
              w
            _ -> pure ()
     in w
  pure
    t
      { closeTransport =
          do closeTransport t
             atomically $ writeTQueue q Nothing
      , sendData = atomically . writeTQueue q . Just
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
