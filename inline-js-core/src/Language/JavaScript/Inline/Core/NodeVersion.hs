module Language.JavaScript.Inline.Core.NodeVersion where

import Control.Exception
import Control.Monad
import Data.List
import Data.Version
import Language.JavaScript.Inline.Core.Exception
import Language.JavaScript.Inline.Core.Utils
import System.Process

parseNodeVersion :: String -> Version
parseNodeVersion s0 = Version vs $ case tag of
  "" -> []
  _ -> [tag]
  where
    vs_ts = split (== '-') $ tail s0
    vs = map read $ split (== '.') $ head vs_ts
    tag = intercalate "-" $ tail vs_ts

isSupportedVersion :: Version -> Bool
isSupportedVersion v =
  (v >= makeVersion [12, 0, 0])
    || (v >= makeVersion [10, 20, 0] && v < makeVersion [11])

checkNodeVersion :: FilePath -> IO ()
checkNodeVersion p = do
  v <-
    parseNodeVersion
      <$> readProcess p ["--eval", "process.stdout.write(process.version)"] ""
  unless (isSupportedVersion v) $ throwIO NodeVersionUnsupported
