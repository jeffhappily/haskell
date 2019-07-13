module Interview where

-- given an array of integer, find the difference in index of the next bigger int in the array
-- [8, 5, 3, 9 ] => output [3, 2, 1, -1] since 8 index 0, and 9 next bigger is 3 (3-0 =3), if no bigger then -1

q1 :: [Int] -> [Int]
q1 []     = []
q1 xs = map snd $ scanr go [] xs
  where
    go x [] = [(x, 0)]
    go x [y] = if x > y then [(x)]


-- Calculate min cost
q2 :: [Int] -> Int
q2 [] = 0
q2 (x:xs) = min 25 $ min oneDay sevenDays
    where
        oneDay = 2 + q2 xs
        sevenDays = 7 + (q2 $ dropWhile (< x + 7) xs)

main = do
    putStrLn $ show $ q2 [1,4,6,7,28,30]
    putStrLn $ show $ q2 [1,5,7,8,9,11,15]
    putStrLn $ show $ q2 [1,3,5,7,9,10,11,12,13,14,15,16,22,23,24,25]
    putStrLn $ show $ q2 [1,3,5,9,10,14,16,22,23,25,28,29,30]