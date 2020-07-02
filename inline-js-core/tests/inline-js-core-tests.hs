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
      [ assertionWithSession "Session: new/close" $ \_ -> pure (),
        assertionWithSession "Session: delay" $ \s -> do
          vs <-
            replicateM 0x1000 $
              evalNone s "new Promise((resolve) => setTimeout(resolve, 1000))"
          for_ vs evaluate,
        assertionWithSession "Session: roundtrip" $ \s -> do
          let buf = "asdf"
          buf' <- evalBuffer s $ buffer buf
          buf @=? buf',
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
          $ \left_pad_get ->
            withResource
              ( do
                  left_pad <- left_pad_get
                  newSession
                    defaultConfig
                      { nodeModules = Just $ left_pad </> "node_modules"
                      }
              )
              closeSession
              $ \session_get -> testCase "left-pad" $ do
                s <- session_get
                _ <- evaluate =<< evalJSVal s "require('left-pad')"
                pure ()
      ]

withSession :: (IO Session -> TestTree) -> TestTree
withSession = withResource (newSession defaultConfig) closeSession

assertionWithSession :: TestName -> (Session -> Assertion) -> TestTree
assertionWithSession name cont =
  withSession $ \session_get -> testCase name $ cont =<< session_get

withTmpDir :: (FilePath -> IO ()) -> (IO FilePath -> TestTree) -> TestTree
withTmpDir pre =
  withResource
    ( do
        _tmp <- getTemporaryDirectory
        _r <- createTempDirectory _tmp "inline-js"
        pre _r
        pure _r
    )
    removeDirectoryRecursive
