{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tests.Evaluation
  ( tests
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.JavaScript.Inline.Message.Class
import Language.JavaScript.Inline.Message.Eval
import Language.JavaScript.Inline.MessageCounter
import Language.JavaScript.Inline.Session
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)
import Tests.Helpers.Message

tests :: IO TestTree
tests =
  testSpec "JSCode Evaluation" $
  it "Should Handle Many Mixed-Async-and-Sync Requests" $
  withJSSession defJSSessionOpts $ \s -> do
    let requestTest ::
             Request (EvalRequest r)
          => (EvalRequest r, EvalResponse r -> IO ())
          -> IO (MsgId, EvalResponse r -> IO ())
        requestTest (request, test) = do
          msgId <- sendMsg s request
          pure (msgId, test)
        recvAndRunTest (msgId, test) = do
          result <- recvMsg s msgId
          test result
    testPairs <-
      traverse
        requestTest
        [ ( asyncEvaluation
              "import('fs').then(fs => fs.readFileSync.toString())"
          , \r -> isError r `shouldBe` False)
        , (syncEvaluation "while(true){}" `withEvalTimeout` 1000, failsToReturn)
        , (syncEvaluation "BOOM", failsToReturn)
        , ( syncEvaluation "let x = 6*7; JSON.stringify(null)"
          , successfullyReturns Null)
        , (syncEvaluation "JSON.stringify(x)", successfullyReturns $ Number 42)
        , ( syncEvaluation "JSON.stringify(\"left\" + \"pad\")"
          , successfullyReturns $ String "leftpad")
        , (asyncEvaluation "Promise.reject('BOOM')", failsToReturn)
        , ( asyncEvaluation "Promise.resolve(JSON.stringify(x))"
          , successfullyReturns $ Number 42)
        , ( asyncEvaluation
              "new Promise((resolve, _) => setTimeout(resolve, 10000))" `withResolveTimeout`
            1000
          , failsToReturn)
        ]
    traverse_ recvAndRunTest testPairs

failsToReturn :: EvalResponse r -> IO ()
failsToReturn r = isError r `shouldBe` True

successfullyReturns :: Value -> EvalResponse LBS.ByteString -> IO ()
successfullyReturns expected r = do
  isError r `shouldBe` False
  decode' (evalResult r) `shouldBe` Just expected

isError :: EvalResponse r -> Bool
isError EvalError {} = True
isError _ = False
