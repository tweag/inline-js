{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.JavaScript.Inline.Core.NodeVersion
  ( checkNodeVersion
  ) where

import Control.Monad
import Data.Version
import System.Process

split :: (a -> Bool) -> [a] -> [[a]]
split f = foldr w []
  where
    w x acc
      | f x = [] : acc
      | otherwise =
        case acc of
          xs:acc' -> (x : xs) : acc'
          [] -> [[x]]

nodeVersion :: FilePath -> IO Version
nodeVersion p = do
  ('v':s) <- readProcess p ["--version"] ""
  let vs:tags = split (== '-') s
      v = map read $ split (== '.') vs
  pure $ Version v tags

checkNodeVersion :: FilePath -> IO ()
checkNodeVersion p = do
  v <- nodeVersion p
  unless (v >= Version [11] []) $
    fail $ "Detected node version " <> show v <> ", requires at least node 11"
