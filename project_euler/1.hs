import Control.Applicative
import Control.Monad
import System.IO

-- Find the sum of all the multiples of 3 or 5 below N.
main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        let m = n-1
        
        let m3 = sumSeq 3 (m `div` 3) 3
        let m5 = sumSeq 5 (m `div` 5) 5
        let m15 = sumSeq 15 (m `div` 15) 15
        
        putStrLn $ show $ m3 + m5 - m15


getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret
        
sumSeq a n d = n * (2*a + (n-1) * d) `quot` 2