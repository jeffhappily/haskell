module Chap13 where

import           System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
  -> Age
  -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
    Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $ PersonInvalidUnknown $
      "Name was: " ++ show name ++
      " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering

  putStr "Your name: "
  name <- getLine

  putStr "Your age: "
  age' <- getLine

  let age = read age' :: Integer

  let person = mkPerson name age

  case person of
    (Right person) -> putStrLn $ "Yay! Successfully got a person: " ++ show person
    (Left err) -> putStrLn $ "Error occured: " ++ show err
