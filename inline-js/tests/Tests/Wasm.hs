{-# LANGUAGE OverloadedStrings #-}

module Tests.Wasm
  ( tests,
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Language.JavaScript.Inline.Core
import qualified Paths_inline_js
import System.FilePath
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

fib :: Int -> Int
fib n
  | n < 2 = 1
  | otherwise = fib (n - 2) + fib (n - 1)

tests :: IO TestTree
tests = do
  datadir <- Paths_inline_js.getDataDir
  fib_buf <- LBS.readFile $ datadir </> "testdata" </> "fib.wasm"
  pure $ testProperty "WebAssembly" $ over (generate $ const [1 .. 24]) $ \i ->
    monadic $ withJSSession defJSSessionOpts $ \s -> do
      buf_ref <- alloc s fib_buf
      result_ref <-
        eval s $ "WebAssembly.instantiate(" <> takeJSVal buf_ref <> ")"
      fib_result_buf <-
        eval s
          $ jsonStringify
          $ takeJSVal result_ref
            <> ".instance.exports.fib("
            <> fromString (show i)
            <> ")"
      closeJSSession s
      pure $ eitherDecode' fib_result_buf == Right (fib i)
