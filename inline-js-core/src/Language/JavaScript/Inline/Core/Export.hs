{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Export where

import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Dict
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session

-- | The class of Haskell functions which can be exported as JavaScript
-- functions. The Haskell function type should be @a -> b -> .. -> IO r@, where
-- the arguments @a@, @b@, etc are 'FromJS' instances, and the result @r@ is
-- 'ToJS' instance.
class Export f where
  exportArgsFromJS :: Proxy f -> [Dict FromJS]
  exportMonomorphize :: Session -> f -> [LBS.ByteString] -> IO JSExpr

instance ToJS r => Export (IO r) where
  exportArgsFromJS _ = []
  exportMonomorphize _ m [] = toJS <$> m
  exportMonomorphize _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"

instance (FromJS a, Export b) => Export (a -> b) where
  exportArgsFromJS _ =
    Dict (Proxy @a) : exportArgsFromJS (Proxy @b)
  exportMonomorphize s f (x : xs) = do
    a <- fromJS s x
    exportMonomorphize s (f a) xs
  exportMonomorphize _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"
