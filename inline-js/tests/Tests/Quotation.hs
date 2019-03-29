{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tests.Quotation
  ( tests
  ) where

import Control.Monad (forM_)

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Text (pack)
import GHC.Generics (Generic)
import Language.JavaScript.Inline
  ( block
  , defJSSessionOpts
  , expr
  , killJSSession
  , startJSSession
  , withJSSession
  )
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

-- Datatypes used by tests
data GameWorld = GameWorld
  { playerCharacter :: PlayerCharacter
  } deriving (Generic, ToJSON, FromJSON)

data PlayerCharacter = PlayerCharacter
  { hp :: Integer
  , maxHp :: Integer
  , isDead :: Bool
  } deriving (Generic, ToJSON, FromJSON)

-- A test of the initial implementation, with manual Value conversion
tests :: IO TestTree
tests =
  testSpec "Inline JavaScript QuasiQuoter" $ do
    it "should add two numbers and return a Number" $ do
      result <- withJSSession defJSSessionOpts [expr| 1 + 3 |]
      result `shouldBe` (4 :: Int)
    it "should concatenate two Strings" $ do
      result <- withJSSession defJSSessionOpts [expr| "hello" + " goodbye" |]
      result `shouldBe` "hello goodbye"
    it "should take in a Haskell String and return it" $ do
      let sentence = "This is a String"
      result <- withJSSession defJSSessionOpts [expr| $sentence |]
      result `shouldBe` sentence
    it "should append a Haskell-inserted String to a JavaScript String" $ do
      let sentence = "Pineapple"
      result <- withJSSession defJSSessionOpts [expr| $sentence + " on pizza" |]
      result `shouldBe` "Pineapple on pizza"
    it "should work when reusing the same variable in the same splice" $ do
      let myNumber = (4 :: Int)
      result <- withJSSession defJSSessionOpts [expr| $myNumber + $myNumber |]
      result `shouldBe` (8 :: Int)
    it "should compute the result of three separate variables" $ do
      let firstNumber = 1 :: Int
          secondNumber = 2 :: Int
          thirdNumber = 9 :: Int
      result <-
        withJSSession
          defJSSessionOpts
          [expr| $firstNumber + $secondNumber + $thirdNumber |]
      result `shouldBe` (12 :: Int)
    it "should not share state when reusing the same variable across splices" $ do
      let myNumber = 3 :: Int
      result1 <- withJSSession defJSSessionOpts [expr| $myNumber + 3 |]
      result2 <- withJSSession defJSSessionOpts [expr| $myNumber + 4 |]
      result1 `shouldBe` (6 :: Int)
      result2 `shouldBe` (7 :: Int)
    it
      "should not share state when resuing the same variable name across splices" $ do
      let firstOperation =
            let word = pack "Bananas"
             in do result <- withJSSession defJSSessionOpts [expr| $word |]
                   result `shouldBe` word
      let secondOperation =
            let word = pack "Pears"
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
          let word = pack "Bananas"
          result <- [expr| $word |] session
          result `shouldBe` word
      killJSSession session
    it "should process a simple block expression" $ do
      let myNumber = 3 :: Int
      result <-
        withJSSession
          defJSSessionOpts
          [block|
            const myNumber = $myNumber;
            return "Your number was: " + myNumber;
          |]
      result `shouldBe` pack "Your number was: 3"
    it "should process a complex block expression" $ do
      let gameWorld =
            GameWorld
              { playerCharacter =
                  PlayerCharacter {hp = 10, maxHp = 30, isDead = False}
              }
      result <-
        withJSSession
          defJSSessionOpts
          [block|
            const gameWorld = $gameWorld;
            const player = gameWorld.playerCharacter;
            const renderHp = somePlayer => somePlayer.hp + "/" + somePlayer.maxHp;
            if (player.hp <= 0 || player.isDead) {
              return "Game Over";
            } else {
              return "Your HP: " + renderHp(player);
            }
          |]
      result `shouldBe` "Your HP: 10/30"
