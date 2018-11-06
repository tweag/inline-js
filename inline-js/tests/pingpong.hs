{-# LANGUAGE TypeApplications #-}

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

main :: IO ()
main =
  withJSSession defJSSessionOpts $ \s -> do
    r <- evalTo parseJSRefRegion s newJSRefRegion
    quickCheckWith stdArgs {maxSuccess = 512} $
      monadicIO $
      forAllM genValue $ \v ->
        run $ do
          p <- evalJSRef s r $ codeFromValue v
          _recv_v <- eval s $ deRefJSRef r p
          unless (v == _recv_v) $
            fail $ "pingpong: pong mismatch: " <> show (v, _recv_v)
