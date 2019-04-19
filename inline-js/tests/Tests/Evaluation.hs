{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Evaluation
  ( tests
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor
import Language.JavaScript.Inline.Core
import Test.Tasty (TestTree)
import Test.Tasty.Hspec

tests :: IO TestTree
tests =
  testSpec "JSCode Evaluation" $
  it "Should Handle Many Mixed-Async-and-Sync Requests" $
  withJSSession defJSSessionOpts $ \s -> do
    let testPair :: (IO LBS.ByteString, IO LBS.ByteString -> IO ()) -> IO ()
        testPair (c, f) = f c
    traverse_
      testPair
      [ (eval s "import('fs').then(fs => fs.readFileSync.toString())", notError)
      , (evalWithTimeout s (Just 1000) Nothing "while(true){}", isError)
      , (eval s "BOOM", isError)
      , (eval s "let x = 6*7; JSON.stringify(null)", successfullyReturns Null)
      , (eval s "JSON.stringify(x)", successfullyReturns $ Number 42)
      , ( eval s "JSON.stringify(\"left\" + \"pad\")"
        , successfullyReturns $ String "leftpad")
      , (eval s "Promise.reject('BOOM')", isError)
      , ( eval s "Promise.resolve(JSON.stringify(x))"
        , successfullyReturns $ Number 42)
      , ( evalWithTimeout
            s
            Nothing
            (Just 1000)
            "new Promise((resolve, _) => setTimeout(resolve, 10000))"
        , isError)
      ]

successfullyReturns :: Value -> IO LBS.ByteString -> IO ()
successfullyReturns expected c = do
  r <- c
  decode' r `shouldBe` Just expected

isError, notError :: IO a -> IO ()
isError = (`shouldThrow` anyException)

notError = void
