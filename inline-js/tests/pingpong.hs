{-# LANGUAGE TypeApplications #-}

import Control.Monad.Fail
import Data.Int
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Message
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
  withJSSession defJSSessionOpts $ \s ->
    quickCheckWith stdArgs {maxSuccess = 65536} $
    monadicIO $
    forAllM genValue $ \v ->
      run $ do
        _msg_id <- sendMsg s $ Ping v
        _recv_msg <- recvMsg s _msg_id
        case _recv_msg of
          Pong _recv_v
            | v == _recv_v -> pure ()
          _ -> fail $ "pingpong: pong mismatch: " <> show (v, _recv_msg)
