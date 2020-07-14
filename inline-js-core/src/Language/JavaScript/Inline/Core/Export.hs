{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Export where

import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session

-- | The class of Haskell functions which can be exported as JavaScript
-- functions. The Haskell function type should be @a -> b -> .. -> IO r@, where
-- the arguments @a@, @b@, etc are 'FromJS' instances, and the result @r@ is
-- 'ToJS' instance.
class Export f where
  argsToRawJSType :: Proxy f -> [(JSExpr, RawJSType)]
  monomorphize :: Session -> f -> [LBS.ByteString] -> IO JSExpr

instance ToJS r => Export (IO r) where
  argsToRawJSType _ = []
  monomorphize _ m [] = toJS <$> m
  monomorphize _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"

instance (FromJS a, Export b) => Export (a -> b) where
  argsToRawJSType _ =
    (toRawJSType (Proxy @a), rawJSType (Proxy @a)) : argsToRawJSType (Proxy @b)
  monomorphize s f (x : xs) = do
    a <- fromJS s x
    monomorphize s (f a) xs
  monomorphize _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"
