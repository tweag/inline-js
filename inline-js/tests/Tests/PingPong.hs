{-# LANGUAGE TypeApplications #-}

module Tests.PingPong
  ( tests
  ) where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Int
import Data.Maybe
import qualified Data.Text as Text
import GHC.Exts
import Language.JavaScript.Inline.Command
import Language.JavaScript.Inline.JSCode
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
          [ Object . fromList <$> listOf ((,) <$> genString <*> genValue)
          , Array . fromList <$> listOf genValue
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
    withMaxSuccess 1024 $
    monadicIO $ do
      s <- liftIO getSetup
      forAllM genValue $ \v ->
        run $ do
          v_buf_ref <- alloc s $ encode v
          _recv_v <-
            fmap (fromJust . decode') $
            eval s $
            jsonStringify $ jsonParse $ bufferToString $ deRefJSVal v_buf_ref
          unless (v == _recv_v) $
            fail $ "pingpong: pong mismatch: " <> show (v, _recv_v)
          () <- eval s $ freeJSVal v_buf_ref
          pure ()

setup :: IO JSSession
setup = newJSSession defJSSessionOpts

teardown :: JSSession -> IO ()
teardown = closeJSSession
