{-# LANGUAGE ScopedTypeVariables #-}

module Language.JavaScript.Inline.Core.Internal
  ( peekHandle,
    hGetLBS,
    tryAny,
    once,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Foreign
import System.IO
import System.IO.Unsafe

hGet' :: Handle -> Ptr a -> Int -> IO ()
hGet' h p l = do
  l' <- hGetBuf h p l
  unless (l' == l)
    $ fail
    $ "hGet': expected "
      <> show l
      <> " bytes, got "
      <> show l'

{-# INLINE peekHandle #-}
peekHandle :: forall a. Storable a => Handle -> IO a
peekHandle h = alloca $ \p -> do
  hGet' h p (sizeOf (undefined :: a))
  peek p

{-# INLINE hGetLBS #-}
hGetLBS :: Handle -> Int -> IO LBS.ByteString
hGetLBS h l
  | l < 0 = fail $ "hGetLBS: required " <> show l <> " bytes"
  | l == 0 = pure LBS.empty
  | otherwise = fmap LBS.fromStrict $ BS.create l $ \p -> hGet' h p l

{-# INLINE tryAny #-}
tryAny :: IO a -> IO (Either SomeException a)
tryAny m = catch (m >>= (Right <$>) . evaluate) ((Left <$>) . w)
  where
    w err = catch (evaluate err) w

{-# INLINE once #-}
once :: IO a -> IO (IO a)
once m = do
  r <- unsafeInterleaveIO m
  pure $ evaluate r
