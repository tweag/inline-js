{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.JSParse (jsParse) where

import Data.FileEmbed
import Language.JavaScript.Inline.Core
import System.Process

jsParse :: String -> IO (Bool, Bool, [String])
jsParse src =
  do
    let node_path = nodePath defaultConfig
    o <-
      readProcess
        node_path
        [ "-e",
          $( makeRelativeToProject "jsbits/main.js"
               >>= embedStringFile
           )
        ]
        src
    let is_sync_str : is_expr_str : toks = lines o
        !is_sync = read is_sync_str
        !is_expr = read is_expr_str
    pure (is_sync, is_expr, toks)
