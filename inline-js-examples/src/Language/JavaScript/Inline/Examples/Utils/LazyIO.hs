{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Examples.Utils.LazyIO where

import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Internal as LBS
import System.IO.Unsafe

data LazyIO = LazyIO
  { lazyContent :: ~LBS.ByteString,
    lazyOnData :: BS.ByteString -> IO (),
    lazyOnEnd :: IO (),
    lazyOnError :: SomeException -> IO ()
  }

newLazyIO :: IO LazyIO
newLazyIO = do
  q <- newTMQueueIO
  let w = do
        r <- atomically $ readTMQueue q
        case r of
          Just c -> do
            cs <- unsafeInterleaveIO w
            pure $ LBS.Chunk c cs
          _ -> pure LBS.Empty
  s <- unsafeInterleaveIO w
  pure
    LazyIO
      { lazyContent = s,
        lazyOnData = atomically . writeTMQueue q,
        lazyOnEnd = atomically $ closeTMQueue q,
        lazyOnError = \(SomeException err) -> atomically $ do
          writeTMQueue q (throw err)
          closeTMQueue q
      }
