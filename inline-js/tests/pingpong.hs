{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.JavaScript.Inline.Command
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Session
import Prelude hiding (fail)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic

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

instance Arbitrary Value where
  arbitrary = genValue
  shrink = genericShrink

main :: IO ()
main =
  quickCheckWith stdArgs {maxSuccess = 1024} $
  forAllShrink genValue genericShrink $ \v ->
    monadicIO $ do
      (_recv_v, _recv_v') <-
        run $
        withJSSession defJSSessionOpts $ \s -> do
          let c = codeFromValue v
          r <- evalTo parseJSRefRegion s newJSRefRegion
          p <- evalJSRef s r c
          _recv_v <- eval s $ deRefJSRef r p
          _recv_v' <- eval s c
          pure (_recv_v, _recv_v')
      unless (v == _recv_v && v == _recv_v') $
        fail $ "inline-js:pingpong: mismatch: " <> show (v, _recv_v, _recv_v')
