{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

module Language.JavaScript.Inline.Core.JSVal where

import Foreign hiding (new)
import GHC.Exts
import GHC.Types

-- $jsval-notes
--
-- Lifecycle of a 'JSVal':
--   1. If the 'returnType' is specified as 'ReturnJSVal', the eval server makes
--      a new 'JSVal' out of the return value and sends it back.
--   2. The 'JSVal' values can be passed around and later put into a 'JSCode'
--      for evaluation.
--   3. When a 'JSVal' is garbage collected on the client side, the finalizer is
--      called which frees it on the eval server side.
--
-- Notes to keep in mind:
--   1. When putting 'JSVal's into a 'JSCode', ensure the value is used
--      synchronously, as opposed to being used in the function body of a
--      callback. Otherwise, by the time it's actually used, it may have already
--      been freed.
--   2. The finalizer sends a 'JSVal' free message to the eval server. Ensure
--      the free message doesn't appear before the eval message which uses the
--      'JSVal' in the send queue.
--   3. There is no response message for the 'JSVal' free message. Freeing or
--      using a non-existent 'JSVal' should result in a fatal error.

-- | An opaque garbage-collected reference of a JavaScript value. Each 'JSVal'
-- value is associated with a finalizer which frees the reference on the eval
-- server side when invoked.
data JSVal
  = JSVal Word64 (MutVar# RealWorld ())

instance Show JSVal where
  show (JSVal i _) = "JSVal " <> show i

newJSVal :: Word64 -> IO () -> IO JSVal
newJSVal i (IO f) = IO $ \s0 -> case newMutVar# () s0 of
  (# s1, v #) -> case mkWeak# v () f s1 of
    (# s2, _ #) -> (# s2, JSVal i v #)

unsafeUseJSVal :: JSVal -> Word64
unsafeUseJSVal (JSVal i _) = i
