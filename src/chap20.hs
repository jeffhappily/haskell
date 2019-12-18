module Chap20 where

import           Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr min' Nothing
 where
  min' x Nothing  = Just x
  min' x (Just y) = Just $ min x y

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr max' Nothing
 where
  max' x Nothing  = Just x
  max' x (Just y) = Just $ max x y

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

------------

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ b b' b'') = f b <> f b' <> f b''

filterF
  :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs
