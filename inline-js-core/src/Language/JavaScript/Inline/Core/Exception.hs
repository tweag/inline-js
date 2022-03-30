module Language.JavaScript.Inline.Core.Exception where

import Control.Exception

newtype EvalError = EvalError
  { evalErrorMessage :: String
  }
  deriving (Show)

instance Exception EvalError

data SessionClosed
  = SessionClosed
  deriving (Show)

instance Exception SessionClosed
