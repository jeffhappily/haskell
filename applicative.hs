module Applicative where

import           Data.Monoid

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fold (\x z -> Cons (f x) z) Nil as

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f fs) <*> xs  = append (fmap f xs) (fs <*> xs)

main :: IO ()
main = do
  -- instance Monoid a => Applicative ((,) a)
  -- first type argument to two-tuple has to be a monoid
  print $ ("Woo", (+ 1)) <*> (" Hoo!", 0) -- ("Woo Hoo!",1)

  print $ (Sum 2, (+ 1)) <*> (Sum 9, 0) -- (Sum {getSum = 11},1)
