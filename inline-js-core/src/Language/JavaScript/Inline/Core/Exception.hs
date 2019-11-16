{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Core.Exception
  ( InlineJSException (..),
  )
where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Version

data InlineJSException
  = UnsupportedNodeVersion
      { detectedNodeVersion :: Version
      }
  | IllegalResponse
      { illegalResponse :: LBS.ByteString
      }
  | EvalException
      { evalError :: LBS.ByteString
      }
  | SessionClosed
  deriving (Show)

instance Exception InlineJSException
