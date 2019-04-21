{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Command
  ( eval
  , eval'
  , evalWithTimeout
  , evalWithTimeout'
  , alloc
  , alloc'
  , importMJS
  , importMJS'
  , exportHSFunc
  , exportHSFunc'
  , exportSyncHSFunc
  , exportSyncHSFunc'
  ) where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core.HSCode
import Language.JavaScript.Inline.Core.JSCode hiding (importMJS)
import qualified Language.JavaScript.Inline.Core.JSCode as JSCode
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.Message.Eval
import Language.JavaScript.Inline.Core.Session
import Prelude
import System.Directory

checkEvalResponse :: EvalResponse r -> IO r
checkEvalResponse r =
  case r of
    EvalError {..} ->
      fail $
      "Language.JavaScript.Inline.Core.Commands.checkEvalResponse: evaluation failed with " <>
      show evalError
    EvalResult {..} -> pure evalResult

checkEvalResponse' :: IO (EvalResponse r) -> IO r
checkEvalResponse' = (>>= checkEvalResponse)

-- | Runs a 'JSCode' and returns the result. The 'JSCode' evaluation result
-- is called with @Promise.resolve()@, then we return the resolved value
-- back to Haskell.
--
-- Throws in Haskell if the response indicates a failure.
--
-- Explicit type annotation of the returned value is often required.
-- Supported types are:
--
-- 1. @Data.ByteString.Lazy.ByteString@, which indicates the code returns
-- a piece of raw binary data. The data is wrapped by @Buffer.from()@.
--
-- 2. @Language.JavaScript.Inline.Core.JSVal@, which indicates whatever value
-- the code returns is first registered as a @JSCode.JSVal@.
--
-- 3. @()@, which indicates the code doesn't return anything
-- (even if it does, the result is discarded)
eval ::
     forall r.
     ( Request (EvalRequest r)
     , Response (EvalResponse r)
     , ResponseOf (EvalRequest r) ~ (EvalResponse r)
     )
  => JSSession
  -> JSCode
  -> IO r
eval s = join . eval' s

-- | Asynchronous version of 'eval'.
-- Returns the 'IO' action to actually fetch the result.
-- The returned action blocks if the result is not ready yet.
--
-- The point of exporting async versions of APIs is to allow
-- users to improve concurrency without needing to manually
-- fork lots of threads. When performing batch requests, you can
-- first obtain the 'IO' actions, allow requests to be sent to
-- @node@ to be processed concurrently, and retrieve results later.
--
-- All returned 'IO' actions are idempotent. And all sync/async
-- APIs are fully thread-safe; although unnecessary, you can still
-- invoke them in whatever forked thread you created :)
eval' ::
     forall r.
     ( Request (EvalRequest r)
     , Response (EvalResponse r)
     , ResponseOf (EvalRequest r) ~ (EvalResponse r)
     )
  => JSSession
  -> JSCode
  -> IO (IO r)
eval' s = evalWithTimeout' s Nothing Nothing

-- | Like 'eval', while the eval/resolve timeouts can be specified
-- in milliseconds.
evalWithTimeout ::
     forall r.
     ( Request (EvalRequest r)
     , Response (EvalResponse r)
     , ResponseOf (EvalRequest r) ~ (EvalResponse r)
     )
  => JSSession
  -> Maybe Int
  -> Maybe Int
  -> JSCode
  -> IO r
evalWithTimeout s et rt c = join $ evalWithTimeout' s et rt c

evalWithTimeout' ::
     forall r.
     ( Request (EvalRequest r)
     , Response (EvalResponse r)
     , ResponseOf (EvalRequest r) ~ (EvalResponse r)
     )
  => JSSession
  -> Maybe Int
  -> Maybe Int
  -> JSCode
  -> IO (IO r)
evalWithTimeout' s et rt c =
  checkEvalResponse' <$>
  sendMsg
    s
    (EvalRequest {evalTimeout = et, resolveTimeout = rt, evalCode = c} :: EvalRequest r)

-- | Allocates a @Buffer@ and returns the 'JSVal'.
--
-- Throws in Haskell if the response indicates a failure.
alloc :: JSSession -> LBS.ByteString -> IO JSVal
alloc s buf = join $ alloc' s buf

alloc' :: JSSession -> LBS.ByteString -> IO (IO JSVal)
alloc' s buf =
  checkEvalResponse' <$> sendMsg s AllocRequest {allocContent = buf}

-- | @import()@ a @.mjs@ ECMAScript module on the local file system,
-- and returns the 'JSVal' of the module namespace object.
-- It's safe to use a relative path here
-- (relative to the Haskell process, not @node@),
-- since we call 'canonicalizePath' first anyway.
--
-- Throws in Haskell if the response indicates a failure.
importMJS :: JSSession -> FilePath -> IO JSVal
importMJS s p = join $ importMJS' s p

importMJS' :: JSSession -> FilePath -> IO (IO JSVal)
importMJS' s p = do
  p' <- canonicalizePath p
  eval' s $ JSCode.importMJS p'

-- | Exports an 'HSFunc' to JavaScript,
-- returns the JavaScript wrapper function's 'JSVal' and a finalizer to free the 'HSFunc'.
-- When the 'HSFunc' is no longer used,
-- first call 'freeJSVal' to free the JavaScript wrapper function,
-- then call the finalizer to remove the registered 'HSFunc' from the current 'JSSession'.
--
-- Throws in Haskell if the response indicates a failure.
exportHSFunc :: JSSession -> HSFunc -> IO (JSVal, IO ())
exportHSFunc s f = do
  (c, fin) <- exportHSFunc' s f
  v <- c
  pure (v, fin)

exportHSFunc' :: JSSession -> HSFunc -> IO (IO JSVal, IO ())
exportHSFunc' s f = do
  (r, fin) <- newHSFunc s False f
  c <- checkEvalResponse' <$> sendMsg s r
  pure (c, fin)

-- | Like 'exportHSFunc', except the JavaScript function is made synchronous.
-- Very heavy hammer, only use as a last resort,
-- and make sure the result's length doesn't exceed 'nodeSharedMemSize'.
exportSyncHSFunc :: JSSession -> HSFunc -> IO (JSVal, IO ())
exportSyncHSFunc s f = do
  (c, fin) <- exportSyncHSFunc' s f
  v <- c
  pure (v, fin)

exportSyncHSFunc' :: JSSession -> HSFunc -> IO (IO JSVal, IO ())
exportSyncHSFunc' s f = do
  (r, fin) <- newHSFunc s True f
  c <- checkEvalResponse' <$> sendMsg s r
  pure (c, fin)
