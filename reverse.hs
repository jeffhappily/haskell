reverse1 :: [a] -> [a]
reverse1 []     = []
reverse1 (x:xs) = reverse1 xs ++ [x]

reverse2 :: [a] -> [a]
reverse2 xs = reverse2' xs [] where
  reverse2' [] acc     = acc
  reverse2' (x:xs) acc = reverse2' xs (x:acc)
