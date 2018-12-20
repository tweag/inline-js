module Tests.Helpers.Message
  ( syncEvaluation
  , asyncEvaluation
  , withEvalTimeout
  , withResolveTimeout
  ) where

import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message

syncEvaluation :: JSCode -> SendMsg
syncEvaluation code = Eval code Nothing Nothing False

asyncEvaluation :: JSCode -> SendMsg
asyncEvaluation code = Eval code Nothing Nothing True

withEvalTimeout :: SendMsg -> Double -> SendMsg
withEvalTimeout request milliseconds = request {evalTimeout = pure milliseconds}

withResolveTimeout :: SendMsg -> Double -> SendMsg
withResolveTimeout request milliseconds =
  request {resolveTimeout = pure milliseconds}
