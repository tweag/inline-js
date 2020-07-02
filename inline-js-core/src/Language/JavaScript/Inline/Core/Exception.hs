module Language.JavaScript.Inline.Core.Exception where

import Control.Exception

data NodeVersionUnsupported
  = NodeVersionUnsupported
  deriving (Show)

instance Exception NodeVersionUnsupported

newtype EvalError = EvalError
  { evalErrorMessage :: String
  }
  deriving (Show)

instance Exception EvalError

data SessionClosed
  = SessionClosed
  deriving (Show)

instance Exception SessionClosed
