module Chap18 where

import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First  a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second

  First a  <*> _        = First a
  _        <*> First  a = First a
  Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
  return = pure

  First  a >>= _ = First a
  Second b >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

--------------

data Nope a =
  NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  _ <*> _ = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

--------------

data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap _ (PRight b) = PRight b
  fmap f (PLeft  a) = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft

  PRight a <*> _        = PRight a
  _        <*> PRight a = PRight a
  PLeft f  <*> PLeft  a = PLeft $ f a

instance Monad (BahEither b) where
  PRight b >>= _ = PRight b
  PLeft  a >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

--------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

--------------

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil       <*> _   = Nil
  _         <*> Nil = Nil
  Cons f fs <*> xs  = append (fmap f xs) (fs <*> xs)

instance Monad List where
  return = pure

  Nil       >>= _ = Nil
  Cons x xs >>= f = append (f x) (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-----------

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []       _ = return []
meh (x:xs) f   = do
  xs' <- meh xs f

  f x >>= \x' -> return $ x' : xs'

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

main :: IO ()
main = do
  quickBatch
    (monad $ (Second ("hi", 1, [1, 2]) :: Sum String (String, Int, [Int])))

  quickBatch (monad $ (NopeDotJpg :: Nope (String, Int, [Int])))

  quickBatch
    (monad $ (PLeft ("hi", 1, [1, 2]) :: BahEither String (String, Int, [Int])))

  quickBatch
    (monad $ (Identity ("hi", 1, [1, 2]) :: Identity (String, Int, [Int])))

  quickBatch (monad $ (Cons ("hi", 1, [1, 2]) Nil :: List (String, Int, [Int])))
