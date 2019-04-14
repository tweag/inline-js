module Language.JavaScript.Inline.Core.Internal
  ( once
  ) where

import Control.Exception
import System.IO.Unsafe

{-# INLINE once #-}
once :: IO a -> IO (IO a)
once m = do
  r <- unsafeInterleaveIO m
  pure $ evaluate r
