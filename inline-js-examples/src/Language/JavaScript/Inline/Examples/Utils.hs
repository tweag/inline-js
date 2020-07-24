{-# LANGUAGE ScopedTypeVariables #-}

module Language.JavaScript.Inline.Examples.Utils where

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import System.IO.Unsafe

storableToLBS :: Storable a => a -> LBS.ByteString
storableToLBS a =
  unsafeDupablePerformIO $
    fmap LBS.fromStrict $
      BS.create (sizeOf a) $ \p ->
        poke (castPtr p) a

storableFromLBS :: forall a. Storable a => LBS.ByteString -> IO a
storableFromLBS s = BS.unsafeUseAsCStringLen (LBS.toStrict s) $ \(p, len) ->
  if len == sizeOf (undefined :: a)
    then peek (castPtr p)
    else fail "Language.JavaScript.Inline.Examples.Utils.storableFromLBS"
