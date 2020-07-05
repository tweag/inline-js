{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.JavaScript.Inline.Core.Instruction where

import Control.Concurrent
import Control.Exception
import Data.Binary.Get
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.IntSet as IS
import Foreign
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.IPC
import Language.JavaScript.Inline.Core.JSVal
import Language.JavaScript.Inline.Core.Message
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils
import System.IO.Unsafe

evalWithDecoder ::
  JSReturnType ->
  (Session -> LBS.ByteString -> IO a) ->
  Session ->
  JSExpr ->
  IO a
evalWithDecoder _return_type _decoder _session@Session {..} _code =
  do
    _inbox <- newEmptyMVar
    let _cb _resp = case _resp of
          Left _err_buf ->
            putMVar _inbox $ Left $
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
        { requestId = _id,
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

-- | Evaluates a 'JSExpr' and discards the evaluation result.
evalNone :: Session -> JSExpr -> IO ()
evalNone = evalWithDecoder ReturnNone $ \_ _ -> pure ()

-- | Evaluates a 'JSExpr' to a 'LBS.ByteString'. The evaluation result should be
-- an @ArrayBufferView@(@Buffer@, @TypedArray@ or @DataView@) or @ArrayBuffer@.
-- It can also be a @string@, in which case the UTF-8 encoded result is
-- returned. Other JavaScript types will result in an 'EvalError'.
evalBuffer :: Session -> JSExpr -> IO LBS.ByteString
evalBuffer = evalWithDecoder ReturnBuffer $ \_ _result_buf -> pure _result_buf

-- | Evaluate a 'JSExpr', call @JSON.stringify()@ on the evaluation result and
-- return the UTF-8 encoded result.
evalJSON :: Session -> JSExpr -> IO LBS.ByteString
evalJSON = evalWithDecoder ReturnJSON $ \_ _result_buf -> pure _result_buf

-- | Evaluates a 'JSExpr' to a 'JSVal'. The evaluation result can be of any
-- JavaScript type.
evalJSVal :: Session -> JSExpr -> IO JSVal
evalJSVal = evalWithDecoder ReturnJSVal $ \_session _jsval_id_buf -> do
  _jsval_id <- runGetExact getWord64host _jsval_id_buf
  newJSVal _jsval_id (sessionSend _session $ JSValFree _jsval_id)

sessionSend :: Session -> MessageHS -> IO ()
sessionSend Session {..} msg = send ipc $ toLazyByteString $ messageHSPut msg
