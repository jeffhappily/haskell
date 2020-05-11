import Prelude (Maybe(..), Integer, Bool(..), (+))
-- returns the last element
last' :: [a] -> Maybe a
last' [] = Nothing
last' [x] = Just x
last' (x:xs) = last' xs

-- concatenates two list
-- like [a,b,c] ++ [d,e,f] == [a,b,c,d,e,f]
(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

-- find p returns the element in the list that p x is True
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs) = if f x then Just x else find f xs

-- like find but returns all matching elements
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs

-- returns the length of the list
length :: [a] -> Integer
length [] = 0
length (_:xs) = 1 + length xs

-- returns the sum of all list elements
sum :: [Integer] -> Integer
sum [] = 0
sum (x:xs) = x + sum xs
