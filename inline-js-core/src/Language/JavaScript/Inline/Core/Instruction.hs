{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Core.Instruction where

import Control.Concurrent
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
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
evalWithDecoder _return_type _decoder _session _code = do
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
  unsafeInterleaveIO $
    takeMVar _inbox >>= \case
      Left (SomeException _err) -> throwIO _err
      Right _result -> pure _result

exportAsyncOrSync :: forall f. Export f => Bool -> Session -> f -> IO JSVal
exportAsyncOrSync _is_sync _session f = do
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
      { exportIsSync = _is_sync,
        exportRequestId = _id_cb,
        exportFuncId = _id_f,
        argsType = args_type
      }
  unsafeInterleaveIO $
    takeMVar _inbox >>= \case
      Left (SomeException _err) -> throwIO _err
      Right _result -> pure _result
