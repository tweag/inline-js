{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tests.Evaluation
  ( tests
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.Core.MessageCounter
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
        [ ( evaluation "import('fs').then(fs => fs.readFileSync.toString())"
          , \r -> isError r `shouldBe` False)
        , (evaluation "while(true){}" `withEvalTimeout` 1000, failsToReturn)
        , (evaluation "BOOM", failsToReturn)
        , ( evaluation "let x = 6*7; JSON.stringify(null)"
          , successfullyReturns Null)
        , (evaluation "JSON.stringify(x)", successfullyReturns $ Number 42)
        , ( evaluation "JSON.stringify(\"left\" + \"pad\")"
          , successfullyReturns $ String "leftpad")
        , (evaluation "Promise.reject('BOOM')", failsToReturn)
        , ( evaluation "Promise.resolve(JSON.stringify(x))"
          , successfullyReturns $ Number 42)
        , ( evaluation "new Promise((resolve, _) => setTimeout(resolve, 10000))" `withResolveTimeout`
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
