{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import Language.JavaScript.NodeJS.CabalHook

main :: IO ()
main = defaultMainWithEvalServer pure
