module PoemLines where

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls@(x:xs) 
  | c == x = splitBy c xs
  | otherwise = word : splitBy c remaining
  where
    word = takeWhile (/= c) ls
    remaining = drop 1 . dropWhile (/= c) $ ls

myWords :: String -> [String]
myWords = splitBy ' '

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
  \ symmetry?"

sentences = firstSen ++ secondSen
  ++ thirdSen ++ fourthSen

-- Implement this
myLines :: String -> [String]
myLines = splitBy '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences
    == shouldEqual)
