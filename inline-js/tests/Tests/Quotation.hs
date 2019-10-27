{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Quotation
  ( tests,
  )
where

import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
  )
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (pack)
import GHC.Generics (Generic)
import Language.JavaScript.Inline
import Test.Tasty
import Test.Tasty.HUnit

-- Datatypes used by tests
newtype GameWorld
  = GameWorld
      { playerCharacter :: PlayerCharacter
      }
  deriving (Generic, ToJSON, FromJSON)

data PlayerCharacter
  = PlayerCharacter
      { hp :: Integer,
        maxHp :: Integer,
        isDead :: Bool
      }
  deriving (Generic, ToJSON, FromJSON)

-- A test of the initial implementation, with manual Value conversion
tests :: IO TestTree
tests =
  pure $
    testGroup
      "Inline JavaScript QuasiQuoter"
      [ testCase "should add two numbers and return a Number" $ do
          result <- withJSSession defJSSessionOpts [expr| 1 + 3 |]
          result @?= (4 :: Int),
        testCase "should concatenate two Strings" $ do
          result <- withJSSession defJSSessionOpts [expr| "hello" + " goodbye" |]
          result @?= "hello goodbye",
        testCase "should take in a Haskell String and return it" $ do
          let sentence = "This is a String"
          result <- withJSSession defJSSessionOpts [expr| $sentence |]
          result @?= sentence,
        testCase "should append a Haskell-inserted String to a JavaScript String" $
          do
            let sentence = "Pineapple"
            result <-
              withJSSession
                defJSSessionOpts
                [expr| $sentence + " on pizza" |]
            result @?= "Pineapple on pizza",
        testCase "should work when reusing the same variable in the same splice" $
          do
            let myNumber = 4 :: Int
            result <- withJSSession defJSSessionOpts [expr| $myNumber + $myNumber |]
            result @?= (8 :: Int),
        testCase "should compute the result of three separate variables" $ do
          let firstNumber = 1 :: Int
              secondNumber = 2 :: Int
              thirdNumber = 9 :: Int
          result <-
            withJSSession
              defJSSessionOpts
              [expr| $firstNumber + $secondNumber + $thirdNumber |]
          result @?= (12 :: Int),
        testCase
          "should not share state when reusing the same variable across splices"
          $ do
            let myNumber = 3 :: Int
            result1 <- withJSSession defJSSessionOpts [expr| $myNumber + 3 |]
            result2 <- withJSSession defJSSessionOpts [expr| $myNumber + 4 |]
            result1 @?= (6 :: Int)
            result2 @?= (7 :: Int),
        testCase
          "should not share state when resuing the same variable name across splices"
          $ do
            let firstOperation =
                  let word = pack "Bananas"
                   in do
                        result <- withJSSession defJSSessionOpts [expr| $word |]
                        result @?= word
            let secondOperation =
                  let word = pack "Pears"
                   in do
                        result <- withJSSession defJSSessionOpts [expr| $word |]
                        result @?= word
            firstOperation
            secondOperation
            firstOperation,
        testCase
          "should not collide names when reusing the same variable name across splices in the same session"
          $ do
            session <- newJSSession defJSSessionOpts
            forM_ [1 :: Int .. 10] $ const $ do
              let word = pack "Bananas"
              result <- [expr| $word |] session
              result @?= word
            closeJSSession session,
        testCase "should process a simple block expression" $ do
          let myNumber = 3 :: Int
          result <-
            withJSSession
              defJSSessionOpts
              [block|
            const myNumber = $myNumber;
            return "Your number was: " + myNumber;
          |]
          result @?= pack "Your number was: 3",
        testCase "should process a complex block expression" $ do
          let gameWorld = GameWorld
                { playerCharacter = PlayerCharacter
                    { hp = 10,
                      maxHp = 30,
                      isDead = False
                    }
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
          result @?= "Your HP: 10/30",
        testCase "should pass and return a lazy ByteString" $ do
          let buf = LBS.pack "SPICE GIRL WANNABEEE"
          result <- withJSSession defJSSessionOpts $ \s -> do
            () <- [expr| global.x = $buf |] s
            (v :: JSVal) <- [expr| global.x |] s
            [expr| $v |] s
          result @?= buf
      ]
