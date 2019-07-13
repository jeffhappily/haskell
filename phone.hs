module Phone where

import           Data.Char
import           Data.List

type PhoneValues = String

data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

-- -----------------------------------------
-- | 1      | 2 ABC   | 3 DEF   |
-- _________________________________________
-- | 4 GHI  | 5 JKL   | 6 MNO   |
-- -----------------------------------------
-- | 7 PQRS | 8 TUV   | 9 WXYZ  |
-- -----------------------------------------
-- | * ^    | 0 + _   | # .,    |
-- -----------------------------------------

daphone = DaPhone
  [('1', "1"),
    ('2', "abc"),
    ('3', "def"),
    ('4', "ghi"),
    ('5', "jkl"),
    ('6', "mno"),
    ('7', "pqrs"),
    ('8', "tuv"),
    ('9', "wxyz"),
    ('*', "*^"),
    ('0', " +_"),
    ('#', "#.,")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone
  -> Char
  -> [(Digit, Presses)]
reverseTaps (DaPhone kv) c = if isUpper c then shiftKey : ans else ans
  where
    shiftKey = ('*', 1)
    c' = toLower c
    -- Find via key or values
    xs = filter (\x -> (elem c' . snd) x || c' == fst x) kv
    ans = case xs of
      []          -> []
      [(key, vals)] -> case elemIndex c' vals of
        Just times -> [(key, times + 1)]
        -- If c is not in values, means key is the number
        Nothing    -> [(key, length vals + 1)]

cellPhonesDead :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhonesDead daphone = concat . map (reverseTaps daphone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0
