{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.JSParse (jsParse) where

import System.Process
import Data.FileEmbed
import Data.Generics (GenericQ, everything)
import qualified Data.Set as S
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser
import System.FilePath
import Type.Reflection

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
