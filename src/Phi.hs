{-# LANGUAGE DeriveFunctor #-}

module Phi (Privileged, PrivilegedT, runPrivilegedT, MonadPrivileged, liftPHI) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans, lift)

newtype Privileged a = MkPrivileged a
  deriving (Functor, Eq)

instance Monoid a => Monoid (Privileged a) where
  mempty = mempty
  (MkPrivileged x) `mappend` (MkPrivileged y) = MkPrivileged (x `mappend` y)

instance Applicative Privileged where
  pure = MkPrivileged
  (<*>) (MkPrivileged f) = fmap f

instance Monad Privileged where
  (MkPrivileged x) >>= k = k x

instance Show (Privileged a) where
  show _ = "_REDACTED_"

newtype PrivilegedT m a = PrivilegedT { runPrivilegedT :: m (Privileged a) }

mapPrivilegedT :: (m (Privileged a) -> n (Privileged b)) -> PrivilegedT m a -> PrivilegedT n b
mapPrivilegedT f = PrivilegedT . f . runPrivilegedT

instance (Functor m) => Functor (PrivilegedT m) where
    fmap f = mapPrivilegedT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (PrivilegedT m) where
    pure = PrivilegedT . return . MkPrivileged
    mf <*> mx = PrivilegedT $ do
        (MkPrivileged f) <- runPrivilegedT mf
        (MkPrivileged x) <- runPrivilegedT mx
        return (MkPrivileged (f x))

instance (Monad m) => Monad (PrivilegedT m) where
    return = PrivilegedT . return . MkPrivileged
    x >>= f = PrivilegedT $ do
        (MkPrivileged y) <- runPrivilegedT x
        runPrivilegedT (f y)

instance MonadTrans PrivilegedT where
    lift = PrivilegedT . fmap MkPrivileged

instance (MonadIO m) => MonadIO (PrivilegedT m) where
    liftIO = lift . liftIO

class (Monad m) => MonadPrivileged m where
  liftPHI :: Privileged a -> m a

instance MonadPrivileged Privileged where
  liftPHI (MkPrivileged x) = return x

instance (Monad m) => MonadPrivileged (PrivilegedT m) where
  liftPHI (MkPrivileged x) = return x
