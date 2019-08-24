{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Evaluation
  ( tests
  ) where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Data.Functor
import Language.JavaScript.Inline.Core
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests =
  pure
    $ testGroup
        "JSCode Evaluation"
        [ testCase "Should Handle Many Mixed-Async-and-Sync Requests"
            $ withJSSession defJSSessionOpts
            $ \s -> do
              let testPair :: (IO LBS.ByteString, IO LBS.ByteString -> IO ()) -> IO ()
                  testPair (c, f) = f c
              traverse_
                testPair
                [ ( eval s "import('fs').then(fs => fs.readFileSync.toString())",
                    notError
                    ),
                  (evalWithTimeout s (Just 1000) Nothing "while(true){}", isError),
                  (eval s "BOOM", isError),
                  ( eval s "let x = 6*7; JSON.stringify(null)",
                    successfullyReturns Null
                    ),
                  (eval s "JSON.stringify(x)", successfullyReturns $ Number 42),
                  ( eval s "JSON.stringify(\"left\" + \"pad\")",
                    successfullyReturns $ String "leftpad"
                    ),
                  (eval s "Promise.reject('BOOM')", isError),
                  ( eval s "Promise.resolve(JSON.stringify(x))",
                    successfullyReturns $ Number 42
                    ),
                  ( evalWithTimeout
                      s
                      Nothing
                      (Just 1000)
                      "new Promise((resolve, _) => setTimeout(resolve, 10000))",
                    isError
                    )
                  ]
          ]

successfullyReturns :: Value -> IO LBS.ByteString -> IO ()
successfullyReturns expected c = do
  r <- c
  decode' r @?= Just expected

isError, notError :: IO a -> IO ()
isError m = do
  r <- try m
  case r of
    Left (_ :: SomeException) -> pure ()
    Right _ -> fail "isError: not an error"

notError = void
