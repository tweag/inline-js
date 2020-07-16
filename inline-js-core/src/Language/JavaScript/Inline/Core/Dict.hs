{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Language.JavaScript.Inline.Core.Dict where

import Data.Proxy

data Dict c where
  Dict :: c a => Proxy a -> Dict c
