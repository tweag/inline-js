{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.JavaScript.Inline.Core.Command
  ( EvalException (..),
    eval,
    eval',
    alloc,
    alloc',
    importMJS,
    importMJS',
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core.JSCode hiding
  ( importMJS,
  )
import qualified Language.JavaScript.Inline.Core.JSCode as JSCode
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.Message.Eval
import Language.JavaScript.Inline.Core.Session
import System.Directory
import Prelude

-- | The eval server may respond with an error message, in which case this
-- exception is raised.
newtype EvalException
  = EvalException
      { evalErrorMessage :: LBS.ByteString
      }
  deriving (Show)

instance Exception EvalException

checkEvalResponse :: EvalResponse r -> IO r
checkEvalResponse r = case r of
  EvalError {..} -> throwIO EvalException {evalErrorMessage = evalError}
  EvalResult {..} -> pure evalResult

checkEvalResponse' :: IO (EvalResponse r) -> IO r
checkEvalResponse' = (>>= checkEvalResponse)

-- | Runs a 'JSCode' and returns the result. The 'JSCode' evaluation result is
-- called with @Promise.resolve()@, then we return the resolved value back to
-- Haskell.
--
-- Throws in Haskell if the response indicates a failure.
--
-- Explicit type annotation of the returned value is often required. Supported
-- types are:
--
-- 1. @Data.ByteString.Lazy.ByteString@, which indicates the code returns a
--    piece of raw binary data. The data is wrapped by @Buffer.from()@.
--
-- 2. @Language.JavaScript.Inline.Core.JSVal@, which indicates whatever value
--    the code returns is first registered as a @JSCode.JSVal@.
--
-- 3. @()@, which indicates the code doesn't return anything (even if it does,
--    the result is discarded)
eval ::
  forall r.
  (Request (EvalRequest r), Response (EvalResponse r)) =>
  JSSession ->
  JSCode ->
  IO r
eval s = join . eval' s

-- | Asynchronous version of 'eval'. Returns the 'IO' action to actually fetch
-- the result. The returned action blocks if the result is not ready yet.
--
-- The point of exporting async versions of APIs is to allow users to improve
-- concurrency without needing to manually fork lots of threads. When performing
-- batch requests, you can first obtain the 'IO' actions, allow requests to be
-- sent to @node@ to be processed concurrently, and retrieve results later.
--
-- All returned 'IO' actions are idempotent. And all sync/async APIs are fully
-- thread-safe; although unnecessary, you can still invoke them in whatever
-- forked thread you created :)
eval' ::
  forall r.
  (Request (EvalRequest r), Response (EvalResponse r)) =>
  JSSession ->
  JSCode ->
  IO (IO r)
eval' s c =
  checkEvalResponse'
    <$> sendMsg s (EvalRequest {evalCode = c} :: EvalRequest r)

-- | Allocates a @Buffer@ and returns the 'JSVal'.
--
-- Throws in Haskell if the response indicates a failure.
alloc :: JSSession -> LBS.ByteString -> IO JSVal
alloc s buf = join $ alloc' s buf

alloc' :: JSSession -> LBS.ByteString -> IO (IO JSVal)
alloc' s buf =
  checkEvalResponse' <$> sendMsg s AllocRequest {allocContent = buf}

-- | @import()@ a @.mjs@ ECMAScript module on the local file system, and returns
-- the 'JSVal' of the module namespace object. It's safe to use a relative path
-- here (relative to the Haskell process, not @node@), since we call
-- 'canonicalizePath' first anyway.
--
-- Throws in Haskell if the response indicates a failure.
importMJS :: JSSession -> FilePath -> IO JSVal
importMJS s p = join $ importMJS' s p

importMJS' :: JSSession -> FilePath -> IO (IO JSVal)
importMJS' s p = do
  p' <- canonicalizePath p
  eval' s $ JSCode.importMJS p'
