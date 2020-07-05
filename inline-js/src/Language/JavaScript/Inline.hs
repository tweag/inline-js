module Language.JavaScript.Inline
  ( -- * Core functionalities
    module Language.JavaScript.Inline.Core,

    -- * Haskell/JavaScript data marshaling type classes
    ToJS (..),
    FromJS (..),
    Aeson (..),
    EncodedJSON (..),

    -- * Polymorphic eval function
    eval,

    -- * QuasiQuoters for inline JavaScript
    expr,
    block,
  )
where

import Language.JavaScript.Inline.Class
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.TH
