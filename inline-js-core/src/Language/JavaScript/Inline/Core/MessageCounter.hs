{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.JavaScript.Inline.Core.MessageCounter
  ( MsgId (..),
    MsgCounter,
    newMsgCounter,
    newMsgId,
  )
where

import Foreign
import GHC.Exts
import GHC.Types

newtype MsgId
  = MsgId Int
  deriving (Enum, Eq, Integral, Num, Ord, Real, Show)

data MsgCounter
  = MsgCounter (MutableByteArray# RealWorld)

unI# :: Int -> Int#
unI# (I# x) = x

newMsgCounter :: IO MsgCounter
newMsgCounter = IO $ \s0 -> case newByteArray# (unI# (sizeOf (0 :: Int))) s0 of
  (# s1, mba #) -> case writeIntArray# mba 0# 1# s1 of
    s2 -> (# s2, MsgCounter mba #)

newMsgId :: MsgCounter -> IO MsgId
newMsgId (MsgCounter mba) = IO $ \s0 -> case fetchAddIntArray# mba 0# 2# s0 of
  (# s1, x #) -> (# s1, MsgId (I# x) #)
