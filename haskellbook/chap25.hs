{-# LANGUAGE InstanceSigs #-}

module Chap25 where

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose a) = Compose $ (fmap . fmap) f a

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (fmap (<*>) f) <*> a

-- impossible.
-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   return = pure

--   (>>=) :: Compose f g a
--     -> (a -> Compose f g b)
--     -> Compose f g b
--   (Compose a) >>= f = f a

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose xs) = foldMap (foldMap f) xs

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose xs) = Compose <$> traverse (traverse f) xs

---------------------

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b)
    -> (c -> d)
    -> p a c
    -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a

instance Bifunctor Const where
  bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

data Quadriceps a b c d =
  Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz x y z z') = Quadzzz x y (f z) (g z')

-- data Either a b
--   = Left a
--   | Right b

instance Bifunctor Either where
  bimap f _ (Left x)  = Left $ f x
  bimap _ g (Right x) = Right $ g x

---------------

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) =
    Identity (f a)

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) =
    IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) =
    IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure

  (IdentityT ma) >>= f =
    IdentityT $ ma >>= runIdentityT . f
