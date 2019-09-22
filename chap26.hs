{-# LANGUAGE InstanceSigs #-}

module Chap26 where

import           Control.Monad.Trans.Except
import           Data.Tuple

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT (pure (pure x))

  (MaybeT fab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
  => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a
    -> (a -> MaybeT m b)
    -> MaybeT m b
  (MaybeT ma) >>= f =
    MaybeT $ do
      -- ma :: m (Maybe a)
      -- v :: Maybe a
      v <- ma

      case v of
        Nothing -> return Nothing
        Just y  -> runMaybeT (f y)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m
  => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT fab) <*> (EitherT mea) = EitherT $ (<*>) <$> fab <*> mea

instance Monad m
  => Monad (EitherT e m) where
  return = pure

  (EitherT mea) >>= f = EitherT $ do
    v <- mea
    case v of
      Left a  -> return $ Left a
      Right b -> runEitherT $ f b

swapEither :: Either e a -> Either a e
swapEither x = case x of
  Left a  -> Right a
  Right b -> Left b

swapEitherT :: (Functor m)
  => EitherT e m a
  -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m =>
  (a -> m c)
  -> (b -> m c)
  -> EitherT a m b
  -> m c
eitherT f g (EitherT mea) = do
  v <- mea
  case v of
    Left a  -> f a
    Right b -> g b

---------------

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  (ReaderT fab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r

    runReaderT (f a) r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) f' smas
    where f' (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT fab) <*> (StateT smas) = StateT $ \s -> do
    (f, s') <- fab s
    (a, s'') <- smas s'

    return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    (a', s'') <- runStateT (f a) s'

    return (a', s'')

---------------------

embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = MaybeT $ ExceptT $ ReaderT $ return <$> (const (Right (Just 1)))



