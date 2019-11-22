{-# LANGUAGE OverloadedStrings #-}

module Tests.LeftPad
  ( tests,
  )
where

import qualified Data.ByteString.Lazy as LBS
import Distribution.Simple.Utils
import Distribution.Verbosity
import Language.JavaScript.Inline.Core
import qualified Paths_inline_js_core
import System.Directory
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests = do
  datadir <- Paths_inline_js_core.getDataDir
  tmpdir <- getTemporaryDirectory
  pure $
    testGroup
      "left-pad"
      [ testCase "should install left-pad from npm and run it" $ do
          x <-
            withTempDirectory silent tmpdir "inline-js-test-suite-left-pad" $
              \p -> do
                _ <-
                  readCreateProcess
                    ( ( shell
                          ( "npm install "
                              <> (datadir </> "testdata" </> "left-pad-1.3.0.tar")
                          )
                      )
                        { cwd = Just p,
                          std_err = CreatePipe
                        }
                    )
                    ""
                withJSSession defJSSessionOpts {nodeWorkDir = Just p} $ \s ->
                  do
                    mref <- eval s "import('left-pad').then(m => m.default)"
                    eval s $ deRefJSVal mref <> "('foo', 5)"
          x @?= ("  foo" :: LBS.ByteString)
      ]
