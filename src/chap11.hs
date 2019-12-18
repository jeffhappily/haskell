{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chap11 where

import           Data.Char

data Price
  = Price Integer deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
    deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car m _) = Just m
getManu _         = Nothing

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int
  deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (x, y) = tooMany $ x + y

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany $ x + y

type Gardener = String

data Garden
  = Gardenia Gardener
  | Daisy Gardener
  | Rose Gardener
  | Lilac Gardener
  deriving Show


data OperatingSystem
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer
    { os   :: OperatingSystem
    , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = os, lang = lang} | os <- allOperatingSystems, lang <- allLanguages]


isSubseqOf :: (Eq a)
  => [a]
  -> [a]
  -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf x1@(x:xs) (y:ys) = if x == y then isSubseqOf xs ys else isSubseqOf x1 ys


capitalizeWords :: String
  -> [(String, String)]
capitalizeWords xs = map (\w@(x:xs) -> (w, toUpper x : xs)) (words xs)

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph xs = go True xs ""
  where
    go _ "" acc             = acc
    go isStart (' ':xs) acc = go isStart xs (acc ++ " ")
    go _ ('.':xs) acc       = go True xs (acc ++ ".")
    go True (x:xs) acc      = go False xs (acc ++ [toUpper x])
    go False (x:xs) acc     = go False xs (acc ++ [x])
