import Control.Applicative
import Control.Monad
import System.IO

-- What is the largest prime factor of a given number N?
main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int

        putStrLn . show $ getLargestPrimeFactor 1 n


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret


getLargestPrimeFactor :: Int -> Int -> Int

getLargestPrimeFactor i n = 
    if isPrime $ snd m
        then snd m 
        else getLargestPrimeFactor ((fst m) + 1) n
        where m = head [(x, n `div` x) | x <- [i..n], n `mod` x == 0]

isPerfectSquare :: Int -> Bool

isPerfectSquare n = sq * sq == n where sq = floor $ sqrt $ (fromIntegral n)

isPrime :: Int -> Bool

isPrime n
    | even n = False
    | isPerfectSquare n = False
    | n == 3 || n == 5 = True
    | n `mod` 3 == 0 || n `mod` 5 == 0 = False
    | otherwise = null [x | x <- [7, 13..floor $ sqrt $ (fromIntegral n)], odd x, n `mod` x == 0 || n `mod` (x-2) == 0]