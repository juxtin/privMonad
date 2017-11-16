{-# LANGUAGE DeriveFunctor #-}

module Priv (Priv, PrivT, runPrivT, MonadPriv, liftPriv) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Class  (MonadTrans, lift)

newtype Priv a = MkPriv a
  deriving (Functor, Eq)

instance Monoid a => Monoid (Priv a) where
  mempty = mempty
  (MkPriv x) `mappend` (MkPriv y) = MkPriv (x `mappend` y)

instance Applicative Priv where
  pure = MkPriv
  (<*>) (MkPriv f) = fmap f

instance Monad Priv where
  (MkPriv x) >>= k = k x

instance Show (Priv a) where
  show _ = "_REDACTED_"

newtype PrivT m a = PrivT { runPrivT :: m (Priv a) }

mapPrivT :: (m (Priv a) -> n (Priv b)) -> PrivT m a -> PrivT n b
mapPrivT f = PrivT . f . runPrivT

instance (Functor m) => Functor (PrivT m) where
    fmap f = mapPrivT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (PrivT m) where
    pure = PrivT . return . MkPriv
    mf <*> mx = PrivT $ do
        (MkPriv f) <- runPrivT mf
        (MkPriv x) <- runPrivT mx
        return (MkPriv (f x))

instance (Monad m) => Monad (PrivT m) where
    return = PrivT . return . MkPriv
    x >>= f = PrivT $ do
        (MkPriv y) <- runPrivT x
        runPrivT (f y)

instance MonadTrans PrivT where
    lift = PrivT . fmap MkPriv

instance (MonadIO m) => MonadIO (PrivT m) where
    liftIO = lift . liftIO

class (Monad m) => MonadPriv m where
  liftPriv :: Priv a -> m a

instance MonadPriv Priv where
  liftPriv (MkPriv x) = return x

instance (Monad m) => MonadPriv (PrivT m) where
  liftPriv (MkPriv x) = return x
