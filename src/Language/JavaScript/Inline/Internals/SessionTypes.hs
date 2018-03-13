{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.Internals.SessionTypes
  ( JSSource
  , EvalReq(..)
  , EvalResp(..)
  , EvalException(..)
  ) where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T
import Language.JavaScript.Inline.Internals.MsgId
import UnliftIO

type JSSource = T.Text

data EvalReq = EvalReq
  { id :: MsgId
  , code :: JSSource
  }

$(deriveToJSON defaultOptions ''EvalReq)

data EvalResp
  = EvalResult { id :: MsgId
               , result :: Value }
  | EvalError { id :: MsgId
              , error :: Value }

$(deriveFromJSON defaultOptions {sumEncoding = UntaggedValue} ''EvalResp)

data EvalException
  = EvalFailed { code :: JSSource
               , error :: Value }
  | ResultDecodingFailed { code :: JSSource
                         , decodingError :: String }
  deriving (Show)

instance Exception EvalException
