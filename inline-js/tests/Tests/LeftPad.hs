{-# LANGUAGE OverloadedStrings #-}

module Tests.LeftPad
  ( tests
  ) where

import qualified Data.ByteString.Lazy as LBS
import Distribution.Simple.Utils
import Distribution.Verbosity
import Language.JavaScript.Inline.Core
import System.Directory
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

tests :: IO TestTree
tests =
  pure
    $ testGroup
        "left-pad"
        [ testCase "should install left-pad from npm and run it" $ do
            tmpdir <- getTemporaryDirectory
            x <-
              withTempDirectory silent tmpdir "inline-js-test-suite-left-pad" $ \p ->
                do
                  _ <-
                    readCreateProcess
                      ( (shell "npm install left-pad")
                          { cwd = Just p,
                            std_err = CreatePipe
                            }
                        )
                      ""
                  withJSSession defJSSessionOpts {nodeWorkDir = Just p} $ \s -> do
                    mref <- eval s "import('left-pad').then(m => m.default)"
                    eval s $ deRefJSVal mref <> "('foo', 5)"
            x @?= ("  foo" :: LBS.ByteString)
          ]
