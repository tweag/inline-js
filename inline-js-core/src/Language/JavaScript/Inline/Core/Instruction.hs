{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Instruction where

import Control.Concurrent.STM
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Foreign
import Language.JavaScript.Inline.Core.Class
import Language.JavaScript.Inline.Core.Dict
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Export
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils
import System.Directory
import System.IO.Unsafe

evalWithDecoder ::
  RawJSType ->
  (Session -> LBS.ByteString -> IO a) ->
  Session ->
  JSExpr ->
  IO a
evalWithDecoder _return_type _decoder _session@Session {..} _code = do
  _inbox <- newEmptyTMVarIO
  _sp <- newStablePtr _inbox
  let _id = word64FromStablePtr _sp
  sessionSend
    _session
    JSEvalRequest
      { jsEvalRequestId = _id,
        code = _code,
        returnType = _return_type
      }
  touch _code
  unsafeInterleaveIO $ do
    _resp <- atomically $ takeTMVar _inbox `orElse` readTMVar fatalErrorInbox
    freeStablePtr _sp
    case _resp of
      Left _err_buf ->
        throwIO $ EvalError {evalErrorMessage = stringFromLBS _err_buf}
      Right _result_buf -> _decoder _session _result_buf

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
    "((x, f) => (Boolean(x) && typeof x.then === 'function') ? x.then(f) : f(x))("
      <> c
      <> ", "
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

string :: String -> JSExpr
string = toJS . EncodedString . stringToLBS

exportAsyncOrSync :: forall f. Export f => Bool -> Session -> f -> IO JSVal
exportAsyncOrSync _is_sync _session@Session {..} f = do
  _inbox <- newEmptyTMVarIO
  let args_type =
        map (\(Dict p) -> (toRawJSType p, rawJSType p)) $
          exportArgsFromJS (Proxy @f)
      f' = exportMonomorphize _session f
  _sp_inbox <- newStablePtr _inbox
  _sp_f <- newStablePtr f'
  let _id_inbox = word64FromStablePtr _sp_inbox
      _id_f = word64FromStablePtr _sp_f
  sessionSend
    _session
    HSExportRequest
      { exportIsSync = _is_sync,
        exportRequestId = _id_inbox,
        exportFuncId = _id_f,
        argsType = args_type
      }
  unsafeInterleaveIO $ do
    _resp <- atomically $ takeTMVar _inbox `orElse` readTMVar fatalErrorInbox
    freeStablePtr _sp_inbox
    case _resp of
      Left _err_buf ->
        throwIO $ EvalError {evalErrorMessage = stringFromLBS _err_buf}
      Right _jsval_id_buf -> do
        _jsval_id <- runGetExact getWord64host _jsval_id_buf
        newJSVal False _jsval_id $ do
          freeStablePtr _sp_f
          sessionSend _session $ JSValFree _jsval_id

-- | Export a Haskell function as a JavaScript async function, and return its
-- 'JSVal'. Some points to keep in mind:
--
-- * The Haskell function itself can call into JavaScript again via 'eval', and
--   vice versa.
-- * When called in JavaScript, the Haskell function is run in a forked thread.
-- * If the Haskell function throws, the JavaScript function will reject with an
--   @Error@ with the exception string.
-- * Unlike 'JSVal's returned by 'eval', 'JSVal's returned by 'export' are not
--   garbage collected, since we don't know when a function is garbage collected
--   on the @node@ side. These 'JSVal's need to be manually freed using
--   'freeJSVal'.
export :: Export f => Session -> f -> IO JSVal
export = exportAsyncOrSync False

-- | Export a Haskell function as a JavaScript sync function. This is quite
-- heavyweight and in most cases, 'export' is preferrable. 'exportSync' can be
-- useful in certain scenarios when a sync function is desired, e.g. converting
-- a Haskell function to a WebAssembly import.
--
-- Unlike 'export', 'exportSync' has limited reentrancy:
--
-- * The Haskell function may calculate the return value based on the result of
--   calling into JavaScript again, but only synchronous code is supported in
--   this case.
-- * The exported JavaScript sync function must not invoke other exported
--   JavaScript sync functions, either directly or indirectly(Haskell calling
--   into JavaScript again).
exportSync :: Export f => Session -> f -> IO JSVal
exportSync = exportAsyncOrSync True
