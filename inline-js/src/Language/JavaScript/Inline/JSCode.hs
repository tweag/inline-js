module Language.JavaScript.Inline.JSCode
  ( JSCode
  , codeToString
  ) where

import qualified Data.Text.Lazy as LText
import Data.Text.Lazy.Builder
import qualified Language.JavaScript.Inline.JSON as JSON

type JSCode = Builder

codeToString :: JSCode -> JSON.JSString
codeToString = LText.toStrict . toLazyText
