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
      [ testCase "should run left-pad" $ do
          x <- withTempDirectory silent tmpdir "left-pad" $ \p -> do
            callProcess
              "tar"
              ["-C", p, "-x", "-f", datadir </> "testdata" </> "left-pad.tar"]
            withJSSession defJSSessionOpts {nodeWorkDir = Just p} $ \s -> do
              mref <- eval s "import('left-pad').then(m => m.default)"
              eval s $ deRefJSVal mref <> "('foo', 5)"
          x @?= ("  foo" :: LBS.ByteString)
      ]
