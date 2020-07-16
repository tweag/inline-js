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

exportAsyncOrSync :: forall f. Export f => Bool -> Session -> f -> IO JSVal
exportAsyncOrSync _is_sync _session@Session {..} f = do
  _inbox <- newEmptyTMVarIO
  let args_type =
        map (\(Dict p) -> (toRawJSType p, rawJSType p)) $
          exportArgsFromJS (Proxy @f)
      f' = monomorphize _session f
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
          sessionSend _session $ JSValFree _jsval_id
          freeStablePtr _sp_f
