module Language.JavaScript.Inline
  ( module Language.JavaScript.Inline.Core,
    ToJSCode (..),
    FromEvalResult (..),
    Aeson (..),
    eval,
    expr,
    block,
  )
where

import Language.JavaScript.Inline.Class
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.TH
