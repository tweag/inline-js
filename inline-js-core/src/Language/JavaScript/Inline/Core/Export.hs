{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Export where

import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session

class Export f where
  argsToRawJSType :: Proxy f -> [JSExpr]
  export :: Session -> f -> [LBS.ByteString] -> IO JSExpr

instance ToJS r => Export (IO r) where
  argsToRawJSType _ = []
  export _ m [] = toJS <$> m
  export _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"

instance (FromJS a, Export b) => Export (a -> b) where
  argsToRawJSType _ = toRawJSType (Proxy @a) : argsToRawJSType (Proxy @b)
  export s f (x : xs) = do
    a <- fromJS s x
    export s (f a) xs
  export _ _ _ = fail "Language.JavaScript.Inline.Core.Export: impossible"
