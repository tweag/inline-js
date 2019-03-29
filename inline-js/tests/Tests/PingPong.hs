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
tests = do
  pure $
    withResource setup teardown $ \getSetup ->
      testProperty "Ping-Pong Matching" $
      withMaxSuccess 65536 $
      monadicIO $ do
        (s, r) <- liftIO getSetup
        forAllM genValue $ \v ->
          run $ do
            p <- evalTo parseJSRef s $ newJSRef r $ codeFromValue v
            _recv_v <- eval s $ deRefJSRef r p
            unless (v == _recv_v) $
              fail $ "pingpong: pong mismatch: " <> show (v, _recv_v)

setup :: IO (JSSession, JSRefRegion)
setup = do
  session <- startJSSession defJSSessionOpts
  region <- evalTo parseJSRefRegion session newJSRefRegion
  pure (session, region)

teardown :: (JSSession, JSRefRegion) -> IO ()
teardown (s, _) = killJSSession s
