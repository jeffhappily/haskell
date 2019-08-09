module Chap18 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  pure = Second

  First a <*> _ = First a
  _ <*> First a = First a
  Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
  return = pure

  First a >>= _ = First a
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
  fmap f (PLeft a)  = PLeft $ f a

instance Applicative (BahEither b) where
  pure = PLeft

  PRight a <*> _ = PRight a
  _ <*> PRight a = PRight a
  PLeft f <*> PLeft a = PLeft $ f a

instance Monad (BahEither b) where
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (monad $ (Second ("hi", 1, [1, 2] ) :: Sum String (String, Int, [Int])))
  quickBatch (monad $ (NopeDotJpg :: Nope (String, Int, [Int])))
