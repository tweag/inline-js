{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import Language.JavaScript.NodeJS.Splices

main :: IO ()
main = do
  (f, t) <- $(splice)
  r <- f "1+1"
  print r
  t
