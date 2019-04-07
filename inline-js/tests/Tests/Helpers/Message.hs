module Tests.Helpers.Message
  ( syncEvaluation
  , asyncEvaluation
  , withEvalTimeout
  , withResolveTimeout
  ) where

import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message.Eval

syncEvaluation :: JSCode -> EvalRequest a
syncEvaluation = EvalRequest False Nothing Nothing

asyncEvaluation :: JSCode -> EvalRequest a
asyncEvaluation = EvalRequest True Nothing Nothing

withEvalTimeout :: EvalRequest a -> Int -> EvalRequest a
withEvalTimeout request milliseconds = request {evalTimeout = pure milliseconds}

withResolveTimeout :: EvalRequest a -> Int -> EvalRequest a
withResolveTimeout request milliseconds =
  request {resolveTimeout = pure milliseconds}
