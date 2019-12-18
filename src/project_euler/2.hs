import Control.Applicative
import Control.Monad
import System.IO

-- By considering the terms in the Fibonacci sequence whose values do not exceed N, find the sum of the even-valued terms.
main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        
        putStrLn $ show $ fib 1 2 n


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret
        
fib a b lim
    | b > lim = if even a then a else 0
    | odd a = fib b (a+b) lim
    | otherwise = a + fib b (a+b) lim