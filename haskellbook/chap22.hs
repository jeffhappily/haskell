{-# LANGUAGE InstanceSigs #-}

module Chap22 where

import           Control.Applicative
import           Data.Char

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

-----------------------

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
  a <- cap
  b <- rev

  return (a, b)

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
  rev >>= \a ->
    cap >>= \b ->
      return (a, b)

---------------------

newtype Reader r a =
  Reader { runReader :: r -> a }

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap f (Reader r) = Reader $ f . r

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb =
    Reader $ \r -> runReader (aRb (ra r)) r

