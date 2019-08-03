module Chap17 where

import           Control.Applicative
import           Data.List                (elemIndex)
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z -- or, (,) <$> y <*> z

------------

x' :: Maybe Int
x' = elemIndex 3 [1, 2, 3, 4, 5]

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' x' y'

------------

xs'' = [1, 2, 3]
ys'' = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs'' ys''

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs'' ys''

summed :: Maybe Integer
summed = sum <$> liftA2 (,) x'' y''

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity

  Identity f <*> Identity x = Identity $ f x

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty

  Constant x <*> Constant y = Constant $ mappend x y

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs in take 3000 l
    ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ repeat x

  ZipList' fs <*> ZipList' xs = ZipList' $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success

  Failure x <*> Failure y = Failure $ x <> y
  Failure x <*> _         = Failure x
  _         <*> Failure x = Failure x
  Success f <*> Success x = Success $ f x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (applicative $ Identity ("hi", 1 :: Int, [1, 2] :: [Int]))

  -- Second type argument is phantom, but has to be of type (a, b, c)
  quickBatch
    (applicative $ (Constant "hi" :: Constant String (String, String, String)))

  quickBatch (applicative $ (ZipList' [("hi", 1 :: Int, [1, 2] :: [Int])]))

  quickBatch
    ( applicative
    $ (Success ("hi", 1, [1, 2]) :: Validation String (String, Int, [Int]))
    )
