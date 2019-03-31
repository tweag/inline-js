{-# LANGUAGE TypeApplications #-}

module Tests.PingPong
  ( tests
  ) where

import Control.Monad hiding (fail)

import Control.Monad.Fail
import Control.Monad.IO.Class (liftIO)
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.JavaScript.Inline.Command
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Session
import Prelude hiding (fail)
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree, withResource)
import Test.Tasty.QuickCheck (testProperty, withMaxSuccess)

genString :: Gen Text.Text
genString = Text.pack <$> listOf (choose ('\x00', '\xFF'))

genValue :: Gen Value
genValue =
  frequency
    [ ( 1
      , oneof
          [ Object . Map.fromList <$> listOf ((,) <$> genString <*> genValue)
          , Array <$> listOf genValue
          ])
    , ( 64
      , oneof
          [ String <$> genString
          , Number . fromIntegral <$> chooseAny @Int32
          , Bool <$> chooseAny
          , pure Null
          ])
    ]

tests :: IO TestTree
tests =
  pure $
  withResource setup teardown $ \getSetup ->
    testProperty "Ping-Pong Matching" $
    withMaxSuccess 65536 $
    monadicIO $ do
      s <- liftIO getSetup
      forAllM genValue $ \v ->
        run $ do
          p <- evalTo parseJSRef s $ newJSRef $ codeFromValue v
          _recv_v <- eval s $ deRefJSRef p
          unless (v == _recv_v) $
            fail $ "pingpong: pong mismatch: " <> show (v, _recv_v)

setup :: IO JSSession
setup = startJSSession defJSSessionOpts

teardown :: JSSession -> IO ()
teardown = killJSSession
