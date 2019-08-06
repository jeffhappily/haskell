module Monad where

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- safeDiv 200 100 >>= (\x ->
-- 	safeDiv 4 x >>= (\y ->
-- 		safeDiv 8 y >>= (\z ->
-- 			safeDiv 16 z)))

-- Syntatic sugar
val :: Maybe Int
val = do
  x <- safeDiv 200 0
  y <- safeDiv 4 x
  z <- safeDiv 8 y
  safeDiv 16 z
