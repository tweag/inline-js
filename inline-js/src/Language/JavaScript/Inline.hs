module Language.JavaScript.Inline
  ( -- * Core functionalities
    module Language.JavaScript.Inline.Core,

    -- * @aeson@ support
    Aeson (..),

    -- * QuasiQuoters for inline JavaScript
    js,
    jsAsync,
  )
where

import Language.JavaScript.Inline.Aeson
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.TH
