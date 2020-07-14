{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core
  ( -- * Session management
    Config (..),
    defaultConfig,
    Session,
    newSession,
    closeSession,

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

    -- * Exporting Haskell functions to JavaScript
    Export,
    export,
    exportSync,

    -- * Exceptions
    NodeVersionUnsupported (..),
    EvalError (..),
    SessionClosed (..),
  )
where

import Data.Proxy
import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Export
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils
import System.Directory

-- | Evaluate a 'JSExpr' and return the result. Evaluation is /asynchronous/.
-- When this function returns, the eval request has been sent to the eval
-- server, but the result may not be sent back yet. The returned value is a
-- thunk, and forcing it to WHNF will block until the result is sent back.
--
-- The caveats of lazy I/O apply here as well. For instance, returning
-- evaluation result from a @with@-style function may cause a use-after-free
-- problem. In case when it's desirable to ensure the evaluation has completed
-- at a certain point, use 'Control.Exception.evaluate' or @BangPatterns@ to
-- force the result value.
--
-- Modeling asynchronousity with laziness enables us to simplify API. Users can
-- easily regain strictness from the lazy API; if we do it the other way around
-- and provide strict-by-default eval functions, we'll need to explicitly
-- decouple sending of eval requests and receiving of eval results, which
-- complicates the API.
--
-- On the eval server side, the result value is @await@ed before further
-- processing. Therefore if it's a @Promise@, the eval result will be the
-- resolved value instead of the @Promise@ itself. If the @Promise@ value needs
-- to be returned, wrap it in another object (e.g. a single-element array).
eval :: forall a. FromJS a => Session -> JSExpr -> IO a
eval s c =
  evalWithDecoder (rawJSType (Proxy @a)) fromJS s $
    "Promise.resolve("
      <> c
      <> ").then("
      <> toRawJSType (Proxy @a)
      <> ")"

-- | Import a CommonJS module file and return its @module.exports@ object. The
-- module file path can be absolute, or relative to the current Haskell process.
-- The imported module will be retained in the @require()@ loader cache.
importCJS :: Session -> FilePath -> IO JSVal
importCJS s p = do
  p' <- makeAbsolute p
  eval s $ "require(" <> string p' <> ")"

-- | Import an ECMAScript module file and return its module namespace object.
-- The module file path can be absolute, or relative to the current Haskell
-- process. The imported module will be retained in the ESM loader cache.
importMJS :: Session -> FilePath -> IO JSVal
importMJS s p = do
  p' <- makeAbsolute p
  eval s $
    "import('url').then(url => import(url.pathToFileURL("
      <> string p'
      <> ")))"

-- | Export a Haskell function as a JavaScript async function, and return its
-- 'JSVal'. Some points to keep in mind:
--
-- * The Haskell function itself can call into JavaScript again via 'eval', and
--   vice versa.
-- * When called in JavaScript, the Haskell function is run in a forked thread.
-- * If the Haskell function throws, the JavaScript function will reject with an
--   @Error@ with the exception string.
-- * Unlike 'JSVal's returned by 'eval', 'JSVal's returned by 'export' is not
--   garbage collected, since we don't know when a function is garbage collected
--   on the @node@ side.
export :: forall f. Export f => Session -> f -> IO JSVal
export = exportAsyncOrSync False

-- | Export a Haskell function as a JavaScript sync function. This is quite
-- heavyweight and in most cases, 'export' is preferrable. 'exportSync' can be
-- useful in certain scenarios when a sync function is desired, e.g. as
-- WebAssembly imports.
--
-- Unlike 'export', 'exportSync' is not reentrant. When the JavaScript function
-- is called, it blocks @node@ until the Haskell function produces the result or
-- throws. If the Haskell function calls into JavaScript again, that call will
-- be blocked infinitely without warning.
exportSync :: forall f . Export f => Session -> f -> IO JSVal
exportSync = exportAsyncOrSync True

string :: String -> JSExpr
string = toJS . EncodedString . stringToLBS
