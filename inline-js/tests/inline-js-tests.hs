{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Control.Monad
import Data.Foldable
import Distribution.Simple.Utils
import Language.JavaScript.Inline.Core
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
                evalNone s "new Promise((resolve) => setTimeout(resolve, 1000))"
            for_ vs evaluate,
        testCase "Session: roundtrip" $
          withDefaultSession $ \s -> do
            let buf = "asdf"
            buf' <- evalBuffer s $ buffer buf
            buf @=? buf',
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
                  _ <- evaluate =<< evalJSVal s "require('left-pad')"
                  pure ()
      ]

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
