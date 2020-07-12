{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.JavaScript.Inline.Core.Instruction where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.IntSet as IS
import Foreign
import Language.JavaScript.Inline.Core.Exception
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
      { evalRequestId = _id,
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
