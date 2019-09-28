removeLargest :: [Int] -> [Int]
removeLargest [] = []
removeLargest xs = removeFirst xs (maximum xs)


removeFirst :: [Int] -> Int -> [Int]
removeFirst [] _     = []
removeFirst (x:xs) a = if x == a then xs else x : removeFirst xs a
