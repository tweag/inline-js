module Tests.Helpers.Message
  ( evaluation
  , withEvalTimeout
  , withResolveTimeout
  ) where

import Language.JavaScript.Inline.Core

evaluation :: JSCode -> EvalRequest a
evaluation = EvalRequest Nothing Nothing

withEvalTimeout :: EvalRequest a -> Int -> EvalRequest a
withEvalTimeout request milliseconds = request {evalTimeout = pure milliseconds}

withResolveTimeout :: EvalRequest a -> Int -> EvalRequest a
withResolveTimeout request milliseconds =
  request {resolveTimeout = pure milliseconds}
