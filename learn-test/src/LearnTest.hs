module LearnTest where

import           Data.Char

data Fool
  = Fulse
  | Frue
  deriving (Eq, Show)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord ""     = ""

