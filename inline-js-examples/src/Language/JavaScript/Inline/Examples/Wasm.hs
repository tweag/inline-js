{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline.Examples.Wasm where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Foreign
import Language.JavaScript.Inline
import Language.JavaScript.Inline.Examples.Utils

newtype I32 = I32 Word32
  deriving (Show, Storable) via Word32

newtype I64 = I64 Word64
  deriving (Show, Storable) via Word64

newtype F32 = F32 Float
  deriving (Show, Storable) via Float

newtype F64 = F64 Double
  deriving (Show, Storable) via Double

instance ToJS I32 where
  toJS x = [expr| $buf.readUInt32LE() |] where buf = storableToLBS x

instance FromJS I32 where
  rawJSType _ = RawBuffer
  toRawJSType _ =
    [expr| x => { const buf = Buffer.allocUnsafe(4); buf.writeUInt32LE(x); return buf; } |]
  fromJS _ = storableFromLBS

instance ToJS I64 where
  toJS x = [expr| $buf.readBigUInt64LE() |] where buf = storableToLBS x

instance FromJS I64 where
  rawJSType _ = RawBuffer
  toRawJSType _ =
    [expr| x => { const buf = Buffer.allocUnsafe(8); buf.writeBigUInt64LE(x); return buf; } |]
  fromJS _ = storableFromLBS

instance ToJS F32 where
  toJS x = [expr| $buf.readFloatLE() |] where buf = storableToLBS x

instance FromJS F32 where
  rawJSType _ = RawBuffer
  toRawJSType _ =
    [expr| x => { const buf = Buffer.allocUnsafe(4); buf.writeFloatLE(x); return buf; } |]
  fromJS _ = storableFromLBS

instance ToJS F64 where
  toJS x = [expr| $buf.readDoubleLE() |] where buf = storableToLBS x

instance FromJS F64 where
  rawJSType _ = RawBuffer
  toRawJSType _ =
    [expr| x => { const buf = Buffer.allocUnsafe(8); buf.writeDoubleLE(x); return buf; } |]
  fromJS _ = storableFromLBS

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

wasmCompile :: Session -> LBS.ByteString -> IO JSVal
wasmCompile _session _module_buf =
  eval _session [expr| WebAssembly.compile($_module_buf) |]

wasmInstantiate :: Session -> JSVal -> JSVal -> IO JSVal
wasmInstantiate _session _module _import_obj =
  eval _session [expr| WebAssembly.instantiate($_module, $_import_obj) |]
