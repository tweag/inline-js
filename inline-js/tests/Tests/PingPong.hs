{-# LANGUAGE TypeApplications #-}

module Tests.PingPong
  ( tests
  ) where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
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
    withMaxSuccess 65536 $
    monadicIO $ do
      s <- liftIO getSetup
      forAllM genValue $ \v ->
        run $ do
          p <-
            evalTo parseJSVal s $
            newJSVal $
            codeFromValueLBS $
            encode $ String $ Text.decodeUtf8 $ LBS.toStrict $ encode v
          _recv_v <- fmap (fromJust . decode') $ eval s $ deRefJSVal p
          unless (v == _recv_v) $
            fail $ "pingpong: pong mismatch: " <> show (v, _recv_v)

setup :: IO JSSession
setup = newJSSession defJSSessionOpts

teardown :: JSSession -> IO ()
teardown = closeJSSession
