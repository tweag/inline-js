{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.Core.EvalServer where

import qualified Data.ByteString as BS
import Language.JavaScript.Inline.Core.Utils
import System.FilePath

{-# NOINLINE evalServerSrc #-}
evalServerSrc :: BS.ByteString
evalServerSrc = $(embedFile $ "jsbits" </> "index.js")
