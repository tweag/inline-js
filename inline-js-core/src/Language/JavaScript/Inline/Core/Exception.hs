module Language.JavaScript.Inline.Core.Exception where

import Control.Exception

data NotThreadedRTS
  = NotThreadedRTS
  deriving (Show)

instance Exception NotThreadedRTS

newtype EvalError = EvalError
  { evalErrorMessage :: String
  }
  deriving (Show)

instance Exception EvalError

data SessionClosed
  = SessionClosed
  deriving (Show)

instance Exception SessionClosed
