{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline.Core.Utils where

import Data.Binary.Get
import Data.Binary.Get.Internal
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Builder.Prim
import Data.ByteString.Builder.Prim.Internal
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Foreign
import GHC.Exts
import GHC.Types
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import System.IO
import System.IO.Unsafe
import Type.Reflection

-- | 'embedFile' takes a file path, and generates a 'BS.ByteString' at
-- compile-time containing the file content. The file path is relative to the
-- package root directory, and should be included as a part of
-- @extra-source-files@ of that package's @.cabal@ metadata.
--
-- We embed the eval server JavaScript source via TH, instead of using
-- @data-files@ at runtime, so that standalone executables using this package
-- should still work fine if the build directory is no longer present.
--
-- A more space-efficient implementation of 'embedFile' which avoids
-- 'StringPrimL' is possible with GHC 8.10+. We stay with 'StringPrimL' to avoid
-- breaking GHC 8.8.
embedFile :: FilePath -> Q Exp
embedFile p = do
  addDependentFile p
  s <- runIO $ BS.readFile p
  let len = BS.length s
  [|
    unsafePerformIO $
      BS.unsafePackAddressLen len $(litE $ stringPrimL $ BS.unpack s)
    |]

-- | Deduplicate an association list in a left-biased manner; if the same key
-- appears more than once, the left-most key/value pair is preserved.
kvDedup :: Ord k => [(k, a)] -> [(k, a)]
kvDedup = M.toList . M.fromListWith (\_ a -> a)

storableGet :: forall a. Storable a => Get a
storableGet = readN (sizeOf (undefined :: a)) $
  \bs -> unsafeDupablePerformIO $ BS.unsafeUseAsCString bs $ peek . castPtr

storablePut :: Storable a => a -> Builder
storablePut = primFixed storableToF

intFromStablePtr :: StablePtr a -> Int
intFromStablePtr = coerce . ptrToIntPtr . castStablePtrToPtr

intToStablePtr :: Int -> StablePtr a
intToStablePtr = castPtrToStablePtr . intPtrToPtr . coerce

word64FromStablePtr :: StablePtr a -> Word64
word64FromStablePtr = fromIntegral . ptrToWordPtr . castStablePtrToPtr

word64ToStablePtr :: Word64 -> StablePtr a
word64ToStablePtr = castPtrToStablePtr . wordPtrToPtr . fromIntegral

stringFromLBS :: LBS.ByteString -> String
stringFromLBS = LT.unpack . LT.decodeUtf8

stringToLBS :: String -> LBS.ByteString
stringToLBS = LT.encodeUtf8 . LT.pack

hGetExact :: Handle -> Int -> IO LBS.ByteString
hGetExact h len_expected = do
  r <- LBS.hGet h len_expected
  let len_actual = fromIntegral $ LBS.length r
  if len_actual == len_expected
    then pure r
    else
      fail $
        "hGetExact: expected "
          <> show len_expected
          <> " bytes, got "
          <> show len_actual

runGetExact :: forall a. Typeable a => Get a -> LBS.ByteString -> IO a
runGetExact g buf = case runGetOrFail g buf of
  Right (LBS.null -> True, _, r) -> pure r
  _ -> fail $ "runGetExact failed on type " <> show (typeRep @a)

{-# NOINLINE touch #-}
touch :: a -> IO ()
touch a = IO $ \s0 -> case touch# a s0 of
  s1 -> (# s1, () #)

split :: (a -> Bool) -> [a] -> [[a]]
split f l = case foldr w [] l of
  [] : r -> r
  r -> r
  where
    w x acc
      | f x = case acc of
        (_ : _) : _ -> [] : acc
        _ -> acc
      | otherwise = case acc of
        [] -> [[x]]
        xs : acc' -> (x : xs) : acc'
