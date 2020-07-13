{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Instruction where

import Control.Concurrent
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.IntSet as IS
import Data.Proxy
import Foreign
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Export
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils
import System.IO.Unsafe

evalWithDecoder ::
  RawJSType ->
  (Session -> LBS.ByteString -> IO a) ->
  Session ->
  JSExpr ->
  IO a
evalWithDecoder _return_type _decoder _session@Session {..} _code = do
  _inbox <- newEmptyMVar
  let _cb _resp = case _resp of
        Left _err_buf ->
          putMVar _inbox $
            Left $
              toException
                EvalError
                  { evalErrorMessage = stringFromLBS _err_buf
                  }
        Right _result_buf -> do
          _result <- _decoder _session _result_buf
          putMVar _inbox $ Right _result
  _sp <- newStablePtr _cb
  let _id = word64FromStablePtr _sp
  sessionSend
    _session
    JSEvalRequest
      { jsEvalRequestId = _id,
        code = _code,
        returnType = _return_type
      }
  touch _code
  atomicModifyIORef' pendingCallbacks $
    \_cbs -> (IS.insert (intFromStablePtr _sp) _cbs, ())
  unsafeInterleaveIO $
    takeMVar _inbox >>= \case
      Left (SomeException _err) -> throwIO _err
      Right _result -> pure _result

-- | Export a Haskell function as a JavaScript async function.
--
-- The Haskell function type should be @a -> b -> .. -> IO r@, where the
-- arguments @a@, @b@, etc are 'Language.JavaScript.Inline.Core.Class.FromJS'
-- instances, and the result @r@ is 'Language.JavaScript.Inline.Core.Class.ToJS'
-- instance.
--
-- The resulting JavaScript async function can be called like normal functions,
-- set up as callback, etc. When called, the exported Haskell function is run in
-- a forked thread. If the Haskell function throws, the JavaScript function will
-- reject with an @Error@ containing the Haskell exception string.
--
-- Unlike ordinary 'JSVal's returned by 'Language.JavaScript.Inline.Core.eval',
-- the 'JSVal' of exported function is not garbage collected.
export :: forall f. Export f => Session -> f -> IO JSVal
export _session@Session {..} f = do
  _inbox <- newEmptyMVar
  let args_type = argsToRawJSType (Proxy @f)
      f' = monomorphize _session f
      _decoder _jsval_id_buf = do
        _jsval_id <- runGetExact getWord64host _jsval_id_buf
        newJSVal _jsval_id (pure ())
      _cb _resp = case _resp of
        Left _err_buf ->
          putMVar _inbox $
            Left $
              toException
                EvalError
                  { evalErrorMessage = stringFromLBS _err_buf
                  }
        Right _result_buf -> do
          _result <- _decoder _result_buf
          putMVar _inbox $ Right _result
  _sp_cb <- newStablePtr _cb
  _sp_f <- newStablePtr f'
  let _id_cb = word64FromStablePtr _sp_cb
      _id_f = word64FromStablePtr _sp_f
  sessionSend
    _session
    HSExportRequest
      { exportRequestId = _id_cb,
        exportFuncId = _id_f,
        argsType = args_type
      }
  atomicModifyIORef' pendingCallbacks $
    \_cbs -> (IS.insert (intFromStablePtr _sp_cb) _cbs, ())
  unsafeInterleaveIO $
    takeMVar _inbox >>= \case
      Left (SomeException _err) -> throwIO _err
      Right _result -> pure _result
