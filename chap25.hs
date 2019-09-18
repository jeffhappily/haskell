{-# LANGUAGE InstanceSigs #-}

module Chap25 where

newtype Identity a = Identity { runIdentity :: a }

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
