{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Language.JavaScript.Inline.Core.JSVal where

import Foreign
import GHC.Exts
import GHC.Types

-- $jsval-notes
--
-- Lifecycle of a 'JSVal':
--   1. If the 'returnType' is specified as 'RawJSVal', the eval server makes
--      a new 'JSVal' out of the return value and sends it back.
--   2. The 'JSVal' values can be passed around and later put into a 'JSExpr'
--      for evaluation.
--   3. When a 'JSVal' is garbage collected on the client side, the finalizer is
--      called which frees it on the eval server side.
--
-- Notes to keep in mind:
--   1. When putting 'JSVal's into a 'JSExpr', ensure the value is used
--      synchronously, as opposed to being used in the function body of a
--      callback. Otherwise, by the time it's actually used, it may have already
--      been freed.
--   2. The finalizer sends a 'JSVal' free message to the eval server. Ensure
--      the free message doesn't appear before the eval message which uses the
--      'JSVal' in the send queue.
--   3. There is no response message for the 'JSVal' free message. Freeing or
--      using a non-existent 'JSVal' should result in a fatal error.

-- | An opaque reference of a JavaScript value. Each 'JSVal' is registered with
-- a finalizer which frees the reference on the @node@ side when it's garbage
-- collected in Haskell. It's also possible to manually free a 'JSVal' using
-- 'freeJSVal'.

type JSVal# = Any

data JSVal
  = JSVal !Word64 JSVal# (Weak# ())

instance Show JSVal where
  show (JSVal i _ _) = "JSVal " <> show i

newJSVal :: Bool -> Word64 -> IO () -> IO JSVal
newJSVal gc i (IO f) = IO $ \s0 -> case newJSVal# s0 of
  (# s1, v #) ->
    if gc
      then case mkWeak# v () f s1 of
        (# s2, w #) -> (# s2, JSVal i v w #)
      else case mkWeak# () () f s1 of
        (# s2, w #) -> (# s2, JSVal i v w #)

unsafeUseJSVal :: JSVal -> Word64
unsafeUseJSVal (JSVal i _ _) = i

freeJSVal :: JSVal -> IO ()
freeJSVal (JSVal _ _ w) = IO $ \s0 -> case finalizeWeak# w s0 of
  (# s1, 0#, _ #) -> (# s1, () #)
  (# s1, _, f #) -> f s1

foreign import prim "stg_newJSValzh" newJSVal# :: State# RealWorld -> (# State# RealWorld, Any #)
