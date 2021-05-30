{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.JSParse (jsParse) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FileEmbed
import Language.JavaScript.Inline.Core
import UnliftIO
import UnliftIO.Process

jsParse :: String -> IO (Bool, Bool, [String])
jsParse src =
  do
    let node_path = nodePath defaultConfig
    o <- withSystemTempFile "main.js" $
      \p h -> do
        hClose h
        BS.writeFile p parserSrc
        readProcess node_path [p] src
    let is_sync_str : is_expr_str : toks = lines o
        !is_sync = read is_sync_str
        !is_expr = read is_expr_str
    pure (is_sync, is_expr, toks)

parserSrc :: ByteString
parserSrc = $(makeRelativeToProject "jsbits/main.js" >>= embedFile)
