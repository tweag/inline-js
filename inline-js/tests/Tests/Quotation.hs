{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Quotation
  ( tests
  ) where

import Control.Monad (forM_)
import Language.JavaScript.Inline
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Session
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

-- A test of the initial implementation, with manual Value conversion
tests :: IO TestTree
tests =
  testSpec "Inline JavaScript QuasiQuoter" $ do
    it "should add two numbers and return a Number" $ do
      result <- withJSSession defJSSessionOpts [expr| 1 + 3 |]
      result `shouldBe` Number 4
    it "should concatenate two Strings" $ do
      result <- withJSSession defJSSessionOpts [expr| "hello" + " goodbye" |]
      result `shouldBe` String "hello goodbye"
    it "should take in a Haskell String and return it" $ do
      let sentence = String "This is a String"
      result <- withJSSession defJSSessionOpts [expr| $sentence |]
      result `shouldBe` sentence
    it "should append a Haskell-inserted String to a JavaScript String" $ do
      let sentence = String "Pineapple"
      result <- withJSSession defJSSessionOpts [expr| $sentence + " on pizza" |]
      result `shouldBe` String "Pineapple on pizza"
    it "should work when reusing the same variable in the same splice" $ do
      let myNumber = Number 4
      result <- withJSSession defJSSessionOpts [expr| $myNumber + $myNumber |]
      result `shouldBe` Number 8
    it "should compute the result of three separate variables" $ do
      let firstName = Number 1
          secondName = Number 2
          thirdName = Number 9
      result <-
        withJSSession
          defJSSessionOpts
          [expr| $firstName + $secondName + $thirdName |]
      result `shouldBe` Number 12
    it "should not share state when reusing the same variable across splices" $ do
      let myNumber = Number 3
      result1 <- withJSSession defJSSessionOpts [expr| $myNumber + 3 |]
      result2 <- withJSSession defJSSessionOpts [expr| $myNumber + 4 |]
      result1 `shouldBe` Number 6
      result2 `shouldBe` Number 7
    it
      "should not share state when resuing the same variable name across splices" $ do
      let firstOperation =
            let word = String "Bananas"
             in do result <- withJSSession defJSSessionOpts [expr| $word |]
                   result `shouldBe` word
      let secondOperation =
            let word = String "Pears"
             in do result <- withJSSession defJSSessionOpts [expr| $word |]
                   result `shouldBe` word
      firstOperation
      secondOperation
      firstOperation
    it
      "should not collide names when reusing the same variable name across splices in the same session" $ do
      session <- startJSSession defJSSessionOpts
      forM_ [1 :: Int .. 10] $
        const $ do
          let word = String "Bananas"
          result <- [expr| $word |] session
          result `shouldBe` word
