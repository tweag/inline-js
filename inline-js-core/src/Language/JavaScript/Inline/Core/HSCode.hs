module Language.JavaScript.Inline.Core.HSCode
  ( HSFunc(..)
  , HSFuncRef(..)
  ) where

import qualified Data.ByteString.Lazy as LBS

-- | The type of Haskell functions which can be exported to JavaScript.
-- When invoked, the JavaScript wrapper function sends back the argument list;
-- each argument is converted to binary with @Buffer.from()@.
-- The Haskell function computes and returns the result in a forked thread.
--
-- Note that the JavaScript wrapper is an async function by default, and returns a
-- @Promise@ which either resolves with a @Buffer@ result,
-- or rejects with an UTF-8 encoded Haskell exception.
newtype HSFunc = HSFunc
  { runHSFunc :: [LBS.ByteString] -> IO LBS.ByteString
  }

newtype HSFuncRef =
  HSFuncRef Int
