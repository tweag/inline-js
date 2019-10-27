{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Core.Message.HSCode
  ( ExportHSFuncRequest (..),
  )
where

import Data.Binary.Put
import Language.JavaScript.Inline.Core.HSCode
import Language.JavaScript.Inline.Core.Message.Class

data ExportHSFuncRequest
  = ExportHSFuncRequest
      { sync :: Bool,
        exportHSFuncRef :: HSFuncRef
      }

instance Request ExportHSFuncRequest where
  putRequest ExportHSFuncRequest {exportHSFuncRef = HSFuncRef r, ..} = do
    putWord32host $ if sync then 5 else 3
    putWord32host $ fromIntegral r
