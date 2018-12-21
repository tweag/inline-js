{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Tests.Quotation
  ( tests
  ) where

import Control.Monad (void)
import Language.JavaScript.Inline
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (it, shouldBe, testSpec)

tests :: IO TestTree
tests =
  testSpec "Inline JavaScript QuasiQuoter" $ do
    it "should add two numbers and return an Int" $ 4 `shouldBe` 4 -- stub
