scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f acc ls = foldr (\a acc@(x:xs) -> f a x : acc) [acc] ls

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f acc ls = foldl (\acc a -> acc ++ [f (last acc) a]) [acc] ls
