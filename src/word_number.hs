module WordNumber where

import Data.List (intersperse)

digitWords = 
  [ "zero"
  , "one"
  , "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  ]

  
digitToWord :: Int -> String
digitToWord n 
  | n >= length digitWords = ""
  | otherwise = digitWords !! n

digits :: Int -> [Int]
digits n = go n []
  where
    go 0 xs = xs
    go n xs = go (n `div` 10) (n `mod` 10 : xs)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . map (digitToWord) . digits $ n
