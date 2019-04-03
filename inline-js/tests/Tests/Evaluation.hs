{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tests.Evaluation
  ( tests
  ) where

import Data.Aeson
import Data.Foldable
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)
import Tests.Helpers.Message

tests :: IO TestTree
tests =
  testSpec "JSCode Evaluation" $
  it "Should Handle Many Mixed-Async-and-Sync Requests" $
  withJSSession defJSSessionOpts $ \s -> do
    let requestTest (request, test) = do
          msgId <- sendMsg s request
          pure (msgId, test)
        recvAndRunTest (msgId, test) = do
          result <- recvMsg s msgId
          test result
    testPairs <-
      traverse
        requestTest
        [ ( asyncEvaluation "import('fs')"
          , \Result {isError} -> isError `shouldBe` False)
        , (syncEvaluation "while(true){}" `withEvalTimeout` 1000, failsToReturn)
        , (syncEvaluation "BOOM", failsToReturn)
        , (syncEvaluation "undefined", successfullyReturns Null)
        , (syncEvaluation "let x = 6*7", successfullyReturns Null)
        , (syncEvaluation "x", successfullyReturns $ Number 42)
        , ( syncEvaluation "\"left\" + \"pad\""
          , successfullyReturns $ String "leftpad")
        , (asyncEvaluation "Promise.reject('BOOM')", failsToReturn)
        , ( asyncEvaluation "Promise.resolve(x)"
          , successfullyReturns $ Number 42)
        , ( asyncEvaluation
              "new Promise((resolve, _) => setTimeout(resolve, 10000))" `withResolveTimeout`
            1000
          , failsToReturn)
        ]
    traverse_ recvAndRunTest testPairs

failsToReturn :: RecvMsg -> IO ()
failsToReturn Result {isError} = isError `shouldBe` True

successfullyReturns :: Value -> RecvMsg -> IO ()
successfullyReturns expected Result {isError, result} = do
  isError `shouldBe` False
  decode' result `shouldBe` Just expected
