{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.JavaScript.Inline.MonadJS
  ( MonadJS(..)
  , JST
  , runJST
  ) where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Data.Aeson
import Language.JavaScript.Inline.Configure
import Language.JavaScript.Inline.Session (JSSource, Session)
import qualified Language.JavaScript.Inline.Session as S
import UnliftIO

class MonadJS m where
  eval :: FromJSON a => JSSource -> m a

newtype JST m a = JST
  { unJST :: ReaderT Session m a
  }

{-# INLINE runJST #-}
runJST :: MonadUnliftIO m => ConfigureOptions -> JST m a -> m a
runJST opts m = do
  s <- S.newSession opts
  finally (runReaderT (unJST m) s) (S.closeSession s)

deriving instance Functor m => Functor (JST m)

deriving instance Applicative m => Applicative (JST m)

deriving instance Monad m => Monad (JST m)

deriving instance MonadIO m => MonadIO (JST m)

deriving instance MonadFail m => MonadFail (JST m)

deriving instance MonadTrans JST

deriving instance MonadFix m => MonadFix (JST m)

deriving instance MonadZip m => MonadZip (JST m)

deriving instance Alternative m => Alternative (JST m)

deriving instance MonadPlus m => MonadPlus (JST m)

deriving instance MonadCont m => MonadCont (JST m)

deriving instance MonadError e m => MonadError e (JST m)

deriving instance Monad m => MonadReader Session (JST m)

deriving instance MonadState s m => MonadState s (JST m)

deriving instance MonadWriter w m => MonadWriter w (JST m)

instance MonadUnliftIO m => MonadUnliftIO (JST m) where
  {-# INLINE askUnliftIO #-}
  askUnliftIO =
    JST $ do
      u <- askUnliftIO
      pure $ UnliftIO $ unliftIO u . unJST

instance MonadIO m => MonadJS (JST m) where
  {-# INLINE eval #-}
  eval js_src = do
    s <- ask
    S.eval s js_src
