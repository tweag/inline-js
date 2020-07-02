{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline.Core
  ( -- * Session management
    Config (..),
    defaultConfig,
    Session,
    newSession,
    closeSession,

    -- * Evaluation requests
    JSCode,
    JSVal,
    code,
    buffer,
    string,
    json,
    jsval,

    -- * Performing evaluation
    -- $notes-eval
    evalNone,
    evalBuffer,
    evalJSON,
    evalJSVal,
    importCJS,
    importMJS,

    -- * Exceptions
    NodeVersionUnsupported (..),
    EvalError (..),
    SessionClosed (..),
  )
where

import qualified Data.ByteString.Lazy as LBS
import Data.String
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Instruction
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message hiding
  ( code,
  )
import Language.JavaScript.Inline.Core.Session
import System.Directory

-- | Convert a 'String' to 'JSCode'. In most cases, using the 'IsString'
-- instance with the @OverloadedStrings@ extension is more convenient.
code :: String -> JSCode
code = fromString

-- | Embed a 'LBS.ByteString' as a @Buffer@ expression.
buffer :: LBS.ByteString -> JSCode
buffer = JSCode . pure . BufferLiteral

-- | Embed a 'String' as a @string@ expression.
string :: String -> JSCode
string = JSCode . pure . StringLiteral

-- | Embed a UTF-8 encoded JSON expression. It will be parsed with
-- @JSON.parse()@.
json :: LBS.ByteString -> JSCode
json = JSCode . pure . JSONLiteral

-- | Embed a 'JSVal' as an expression.
jsval :: JSVal -> JSCode
jsval = JSCode . pure . JSValLiteral

-- $notes-eval
--
-- The following functions all perform /asynchronous/ evaluation. When they
-- return, the eval request has been sent to the eval server, but the result may
-- not be sent back yet. The returned value is a thunk, and forcing it to WHNF
-- will block until the result is sent back.
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

-- | Import a CommonJS module file and return its @module.exports@ object. The
-- module file path can be absolute, or relative to the current Haskell process.
-- The imported module will be retained in the @require()@ loader cache.
importCJS :: Session -> FilePath -> IO JSVal
importCJS s p = do
  p' <- makeAbsolute p
  evalJSVal s $ "require(" <> string p' <> ")"

-- | Import an ECMAScript module file and return its module namespace object.
-- The module file path can be absolute, or relative to the current Haskell
-- process. The imported module will be retained in the ESM loader cache.
importMJS :: Session -> FilePath -> IO JSVal
importMJS s p = do
  p' <- makeAbsolute p
  evalJSVal s $
    "import('url').then(url => import(url.pathToFileURL("
      <> string p'
      <> ")))"
