{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Exception
import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Distribution.Simple.Utils
import Language.JavaScript.Inline
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
    testGroup
      "kitchen-sink"
      [ testCase "Session: new/close" $ withDefaultSession $ \_ -> pure (),
        testCase "Session: delay" $
          withDefaultSession $ \s -> do
            vs <-
              replicateM 0x1000 $
                (eval @()) s "new Promise((resolve) => setTimeout(resolve, 1000))"
            for_ vs evaluate,
        testCase "Session: roundtrip" $
          withDefaultSession $ \s -> do
            let buf = "asdf"
            buf' <- (eval @LBS.ByteString) s $ toJS buf
            buf' @?= buf,
        testCase "left-pad" $
          withTmpDir
            ( \p -> do
                (ec, _, _) <-
                  readCreateProcessWithExitCode
                    ((shell "npm install left-pad") {cwd = Just p})
                    ""
                case ec of
                  ExitSuccess -> pure ()
                  _ -> fail $ "npm install left-pad failed with " <> show ec
            )
            $ \left_pad ->
              withSession
                defaultConfig
                  { nodeModules = Just $ left_pad </> "node_modules"
                  }
                $ \s -> do
                  _ <- evaluate =<< (eval @JSVal) s "require('left-pad')"
                  pure (),
        testCase "expr" $
          withDefaultSession $ \s -> do
            let x = I 6
                y = I 7
            r <- eval s [expr| $x * $y |]
            r @?= I 42,
        testCase "import" $
          withDefaultSession $ \s -> do
            v <- eval s [expr| (x, y) => x * y |]
            let f = importJSFunc s v
            r <- f (I 6) (I 7)
            r @?= I 42,
        testCase "export" $
          withDefaultSession $ \s -> do
            let f :: V -> V -> IO V
                f (V x) (V y) = pure $ V $ A.Array [x, y]
            v <- export s f
            let x = V $ A.String "asdf"
                y = V $ A.String "233"
            r <- eval s [expr| $v($x, $y) |]
            r @?= V (A.Array [A.String "asdf", A.String "233"])
            freeJSVal v,
        testCase "exportSync" $
          withDefaultSession $ \s -> do
            let f :: V -> V -> IO V
                f (V x) (V y) = pure $ V $ A.Array [x, y]
            v <- exportSync s f
            let x = V $ A.String "asdf"
                y = V $ A.String "233"
            r <- eval s [expr| $v($x, $y) |]
            r @?= V (A.Array [A.String "asdf", A.String "233"])
            freeJSVal v
      ]

newtype I = I Int
  deriving (Eq, Show)
  deriving (ToJS, FromJS) via (Aeson Int)

newtype V = V A.Value
  deriving (Eq, Show)
  deriving (ToJS, FromJS) via (Aeson A.Value)

withSession :: Config -> (Session -> Assertion) -> Assertion
withSession conf = bracket (newSession conf) closeSession

withDefaultSession :: (Session -> Assertion) -> Assertion
withDefaultSession = withSession defaultConfig

withTmpDir :: (FilePath -> IO ()) -> (FilePath -> Assertion) -> Assertion
withTmpDir pre =
  bracket
    ( do
        tmpdir <- getTemporaryDirectory
        p <- createTempDirectory tmpdir "inline-js"
        pre p `onException` removePathForcibly p
        pure p
    )
    removePathForcibly
