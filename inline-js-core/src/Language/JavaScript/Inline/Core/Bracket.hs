{-# LANGUAGE ScopedTypeVariables #-}

-- | Adapted from @unliftio@.
module Language.JavaScript.Inline.Core.Bracket
  ( bracket,
  )
where

import Control.Exception (SomeException (..))
import qualified Control.Exception as EUnsafe

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing = EUnsafe.mask $ \restore -> do
  x <- before
  res1 <- EUnsafe.try $ restore $ thing x
  case res1 of
    Left (e1 :: SomeException) -> do
      _ :: Either SomeException b <-
        EUnsafe.try $ EUnsafe.uninterruptibleMask_ $ after x
      EUnsafe.throwIO e1
    Right y -> do
      _ <- EUnsafe.uninterruptibleMask_ $ after x
      return y
