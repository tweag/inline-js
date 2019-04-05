module Tests.Helpers.Message
  ( syncEvaluation
  , asyncEvaluation
  , withEvalTimeout
  , withResolveTimeout
  ) where

import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message.Eval

syncEvaluation :: JSCode -> EvalRequest
syncEvaluation = EvalRequest False Nothing Nothing

asyncEvaluation :: JSCode -> EvalRequest
asyncEvaluation = EvalRequest True Nothing Nothing

withEvalTimeout :: EvalRequest -> Int -> EvalRequest
withEvalTimeout request milliseconds = request {evalTimeout = pure milliseconds}

withResolveTimeout :: EvalRequest -> Int -> EvalRequest
withResolveTimeout request milliseconds =
  request {resolveTimeout = pure milliseconds}
