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

myLiftA2 :: Applicative f =>
  (a -> b -> c)
  -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks = Reader
