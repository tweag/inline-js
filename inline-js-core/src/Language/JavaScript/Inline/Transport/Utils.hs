{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Transport.Utils
  ( lockSend
  , uniqueRecv
  , strictTransport
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import qualified Data.IntMap.Strict as IMap
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

uniqueRecv ::
     (LBS.ByteString -> Int)
  -> Transport
  -> IO (Int -> IO LBS.ByteString, Transport)
uniqueRecv mk t = do
  mv <- newTVarIO IMap.empty
  ev <- newEmptyTMVarIO
  void $
    forkIO $
    let w = do
          ebuf <- try @SomeException $ recvData t
          case ebuf of
            Right buf -> do
              atomically $ modifyTVar' mv $ IMap.insert (mk buf) buf
              w
            Left err -> do
              atomically $ putTMVar ev err
              pure ()
     in w
  pure
    ( \k -> do
        x <-
          atomically $ do
            m <- readTVar mv
            case IMap.updateLookupWithKey (\_ _ -> Nothing) k m of
              (Just r, m') -> do
                writeTVar mv m'
                pure $ Right r
              _ -> do
                me <- tryReadTMVar ev
                case me of
                  Just e -> pure $ Left e
                  _ -> retry
        case x of
          Left e -> throwIO e
          Right r -> pure r
    , t
        { recvData =
            fail
              "Language.JavaScript.Inline.Transport.Utils.uniqueRecv: recvData is disabled for this Transport"
        })

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
