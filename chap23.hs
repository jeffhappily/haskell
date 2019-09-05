module Chap23 where

import           Control.Monad.Trans.State
import qualified Data.DList                as DL

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer
  -> Integer
  -> [String]
fizzbuzzFromTo from to
  | from > to = []
  | otherwise = fizzBuzz from : fizzbuzzFromTo (from + 1) to

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList [1..100]
