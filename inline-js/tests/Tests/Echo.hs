{-# LANGUAGE OverloadedStrings #-}

module Tests.Echo
  ( tests,
  )
where

import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline.Core
import qualified Paths_inline_js
import System.FilePath
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

genLBS :: Gen LBS.ByteString
genLBS = LBS.pack <$> vectorOf 1024 arbitrary

tests :: IO TestTree
tests = do
  datadir <- Paths_inline_js.getDataDir
  pure
    $ testProperty "Echo"
    $ withMaxSuccess 256
    $ monadicIO
    $ forAllM genLBS
    $ \buf -> run $ withJSSession defJSSessionOpts $ \s -> do
      mod_ref <- importMJS s $ datadir </> "testdata" </> "echo.mjs"
      buf_ref <- alloc s buf
      buf' <-
        eval s $
          deRefJSVal mod_ref
            <> ".identity("
            <> deRefJSVal buf_ref
            <> ")"
      unless (buf' == buf) $ fail "Echo mismatch"
