{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline.Examples.Wasm where

import Control.Exception
import Language.JavaScript.Inline

newtype F64 = F64 Double
  deriving (Show) via Double
  deriving (ToJS, FromJS) via (Aeson Double)

importNew :: Session -> IO JSVal
importNew _session = eval _session [expr| {} |]

importAdd :: Export f => Session -> JSVal -> String -> String -> f -> IO ()
importAdd _session _import_obj (Aeson -> _import_module) (Aeson -> _import_name) _import_hs_func =
  do
    _import_js_func <- exportSync _session _import_hs_func
    evaluate
      =<< eval
        _session
        [block|
          if (!($_import_obj[$_import_module])) {
            $_import_obj[$_import_module] = {};
          }
          $_import_obj[$_import_module][$_import_name] = $_import_js_func;
        |]
