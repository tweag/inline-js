module Language.JavaScript.Inline.Core
  ( -- * Session management
    Config (..),
    defaultConfig,
    Session,
    newSession,
    closeSession,
    killSession,
    withSession,

    -- * Haskell/JavaScript data marshaling
    JSExpr,
    JSVal,
    EncodedString (..),
    EncodedJSON (..),
    RawJSType (..),
    ToJS (..),
    FromJS (..),

    -- * Performing evaluation
    eval,
    importCJS,
    importMJS,

    -- * Importing JavaScript functions to Haskell
    Import,
    importJSFunc,

    -- * Exporting Haskell functions to JavaScript
    Export,
    export,

    -- * Manual resource management
    freeJSVal,

    -- * Exceptions
    EvalError (..),
    SessionClosed (..),
  )
where

import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Export
import Language.JavaScript.Inline.Core.Import
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
