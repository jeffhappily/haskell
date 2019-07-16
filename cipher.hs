module Cipher where

import           Data.Char
import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

bound = 26
steps = 3
keyword = "JEFF"

data ShiftDirection
  = LeftShift
  | RightShift
  deriving Eq

shiftChar :: Char -> Int -> ShiftDirection -> Char
shiftChar c steps dir
  | not $ isAlpha c = c
  | otherwise = chr $ (move start steps) `mod` bound + offset
  where
    start = ord c - offset
    offset = if isUpper c then ord 'A' else ord 'a'
    move = if dir == LeftShift then (-) else (+)

-- Caeser cipher
cipher :: String -> String
cipher ""     = ""
cipher (x:xs) = shiftChar x steps RightShift : cipher xs

-- Vigenere cipher
vCipher :: String -> String
vCipher xs = go xs keyword ""
  where
    go xs "" word         = go xs keyword word
    go "" _ word          = word
    go (x:xs) (y:ys) word = go xs ys (word ++ [shiftChar x (ord y) LeftShift])

vDecipher :: String -> String
vDecipher xs = go xs keyword ""
  where
    go xs "" word         = go xs keyword word
    go "" _ word          = word
    go (x:xs) (y:ys) word = go xs ys (word ++ [shiftChar x (ord y) RightShift])

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "Please select cipher method: "
  putStrLn "1. Caesar"
  putStrLn "2. Vigenere"

  method <- getLine

  case method of
    "1" -> do
      putStr "Please enter text: "

      text <- getLine

      putStrLn $ "Encrypted text: " ++ cipher text

    "2" -> do
      putStr "Please enter text: "

      text <- getLine

      putStrLn $ "Encrypted text: " ++ vCipher text

    _ -> do
      putStrLn "Invalid input, please try again"

      main
