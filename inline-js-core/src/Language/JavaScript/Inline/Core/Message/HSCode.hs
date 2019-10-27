{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Language.JavaScript.Inline.Core.Message.HSCode
  ( ExportHSFuncRequest (..),
  )
where

import Data.Binary.Put
import Language.JavaScript.Inline.Core.HSCode
import Language.JavaScript.Inline.Core.JSCode
import Language.JavaScript.Inline.Core.Message.Class
import Language.JavaScript.Inline.Core.Message.Eval

data ExportHSFuncRequest
  = ExportHSFuncRequest
      { sync :: Bool,
        exportHSFuncRef :: HSFuncRef
      }

instance Request ExportHSFuncRequest where

  type ResponseOf ExportHSFuncRequest = EvalResponse JSVal

  putRequest ExportHSFuncRequest {exportHSFuncRef = HSFuncRef r, ..} = do
    putWord32host $ if sync then 5 else 3
    putWord32host $ fromIntegral r
