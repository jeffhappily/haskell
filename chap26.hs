{-# LANGUAGE InstanceSigs #-}

module Chap26 where

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
