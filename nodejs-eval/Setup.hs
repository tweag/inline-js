{-# OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import Data.Binary
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
    { postConf =
        \args flags pkg_descr lbi -> do
          encodeFile ".buildinfo" $
            datadir (absoluteInstallDirs pkg_descr lbi NoCopyDest)
          postConf simpleUserHooks args flags pkg_descr lbi
    }
