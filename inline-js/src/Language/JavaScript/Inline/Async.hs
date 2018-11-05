{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Language.JavaScript.Inline.Async
  ( Async
  , newAsync
  , killAsync
  , queryAsync
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try . (>>= evaluate)

data Async a = Async
  { asyncThreadId :: ThreadId
  , asyncResult :: MVar (Either SomeException a)
  }

newAsync :: IO a -> IO (Async a)
newAsync m = do
  _box <- newEmptyMVar
  _tid <- forkIO $ tryAny m >>= putMVar _box
  pure Async {asyncThreadId = _tid, asyncResult = _box}

killAsync :: Async a -> IO ()
killAsync Async {..} = do
  f <- isEmptyMVar asyncResult
  when f $ killThread asyncThreadId

queryAsync :: Async a -> IO (Maybe (Either SomeException a))
queryAsync Async {..} = tryReadMVar asyncResult
