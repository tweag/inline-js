{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.JavaScript.Inline.Internals
  ( MsgId(..)
  , newMsgId
  ) where

import Data.Aeson
import Data.Hashable
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Exts
import GHC.Types
import System.IO.Unsafe
import UnliftIO

newtype MsgId = MsgId
  { unMsgId :: Int
  } deriving (Show, Eq, Hashable, FromJSON, ToJSON)

{-# NOINLINE msgIdRef #-}
msgIdRef :: MutableByteArray RealWorld
msgIdRef =
  unsafePerformIO $ do
    mba <- newByteArray $ sizeOf (0 :: Int)
    writeByteArray mba 0 (0 :: Int)
    pure mba

{-# INLINEABLE newMsgId #-}
newMsgId :: MonadIO m => m MsgId
newMsgId =
  liftIO
    (IO
       (\s0 ->
          case fetchAddIntArray# mba 0# 1# s0 of
            (# s1, x #) -> (# s1, MsgId (I# x) #)))
  where
    !(MutableByteArray mba) = msgIdRef
