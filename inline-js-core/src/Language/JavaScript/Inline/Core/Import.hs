{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Language.JavaScript.Inline.Core.Import where

import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Instruction
import Data.List

-- | The class of Haskell functions which can be imported from JavaScript
-- function 'JSVal's. The Haskell function type should be @a -> b -> .. -> IO
-- r@, where the arguments @a@, @b@, etc are 'ToJS' instances, and the result
-- @r@ is 'FromJS' instance.
class Import f where
  importMonomorphize :: Session -> JSVal -> [JSExpr] -> f

instance FromJS r => Import (IO r) where
  importMonomorphize s v xs = eval s $ toJS v <> "(...[" <> mconcat (intersperse "," xs) <> "])"

instance (ToJS a, Import b) => Import (a -> b) where
  importMonomorphize s v xs = \a -> importMonomorphize @b s v (toJS a:xs)

-- | Import a JavaScript function to a Haskell function.
importJSFunc :: Import f => Session -> JSVal -> f
importJSFunc s v = importMonomorphize s v []
