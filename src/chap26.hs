{-# LANGUAGE InstanceSigs #-}

module Chap26 where

import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Reader as R
import           Data.Functor.Identity
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

---------------------

class MonadTrans t where
  -- | Lift a computation from
  -- the argument monad to
  -- the constructed monad.
  lift :: (Monad m) => m a -> t m a

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

-----------------

class (Monad m) => MonadIO m where
  -- | Lift a computation
  -- from the 'IO' monad.
  liftIO :: IO a -> m a

instance (MonadIO m)
  => MonadIO (MaybeT m) where
  liftIO = MaybeT . (Just <$>) . liftIO

instance (MonadIO m)
  => MonadIO (ReaderT r m) where
  liftIO = ReaderT . const . liftIO

instance (MonadIO m)
  => MonadIO (StateT s m) where
  liftIO m = StateT $ (\s -> swap . (,) s <$> liftIO m)

----------------

rDec :: Num a => R.Reader a a
rDec = R.ReaderT $ return . (+ (-1))

rShow :: Show a
  => ReaderT a Identity String
rShow = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a)
  => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn ("Hi: " ++ show r)
  return (r + 1)

sPrintIncAccum :: (Num a, Show a)
  => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn ("Hi: " ++ show s)
  return (show s, s + 1)

-----------------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
  v <- getLine
  guard $ isValid v
  return $ Just v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e ->
      putStrLn
        ("Good, was very excite: " ++ e)

-------------


