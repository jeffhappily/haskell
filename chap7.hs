module Chap7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d)     = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where d  = x `div` 100
        d2 = d `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b  
 | b = x
 | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
