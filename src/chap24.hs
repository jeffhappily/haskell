{-# LANGUAGE QuasiQuotes #-}

module Chap24 where

import           Control.Applicative
import           Control.Monad           (void)
import           Data.Dates
import           Data.List
import           Data.Ratio              ((%))
import           Data.Time.Format
import           Data.Word
import           Text.Parser.Combinators
import           Text.RawString.QQ
import           Text.Read               (readMaybe)
import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1'

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

p123 :: String -> IO ()
p123 s = print $ parseString (string s) mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

---------------

-- type NumberOrFraction = Either Integer Rational

parseNumberOrFraction :: Parser Rational
parseNumberOrFraction =
      try parseFraction
  <|> (%1) <$> decimal

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

-------------------

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSS x) (NOSS y) = compare x y
  compare (NOSI x) (NOSI y) = compare x y
  compare (NOSS x) (NOSI y) = compare x (show y)
  compare (NOSI x) (NOSS y) = compare (show x) y

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer ma mi p r m) (SemVer ma' mi' p' r' m')
    | ma /= ma' = compare ma ma'
    | mi /= mi' = compare mi mi'
    | p /= p' = compare p p'
    | (r == [] || r' == []) && not (r == [] && r' == []) = if r == [] then GT else LT
    | length r /= length r' = compare (length r) (length r')
    | otherwise = foldl (\z x -> if z /= EQ then z else uncurry compare x) EQ $ zip r r'

parseRelease :: Parser Release
parseRelease = char '-' >>
  (do
    str <- some alphaNum
    case readMaybe str :: Maybe Integer of
      Just n  -> return $ NOSI n
      Nothing -> return $ NOSS str
  ) `sepBy` char '.'

parseMetadata :: Parser Metadata
parseMetadata = char '+' >>
  (do
    str <- some alphaNum
    case readMaybe str :: Maybe Integer of
      Just n  -> return $ NOSI n
      Nothing -> return $ NOSS str
  ) `sepBy` char '.'

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  char '.'
  minor <- decimal
  char '.'
  patch <- decimal

  release <- try parseRelease <|> (do return [])
  metadata <- try parseMetadata <|> (do return [])

  return $ SemVer major minor patch release metadata

------------------

parseDigit :: Parser Char
parseDigit = oneOf "1234567890"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = try (char '-' >> negate <$> base10Integer)
             <|> base10Integer

-----------------

-- aka area code
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

skipCountryCode :: Parser ()
skipCountryCode = try (skipOptional (char '1' >> char '-')) <|> (do return ())

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = do
  skipOptional (char '(')
  a <- digit
  b <- digit
  c <- digit
  skipOptional (char ')')
  skipOptional (char '-')
  skipOptional (char ' ')

  return $ read $ a : b : c : []

parseExchange :: Parser Exchange
parseExchange = do
  a <- digit
  b <- digit
  c <- digit
  skipOptional (char '-')

  return $ read $ a : b : c : []

parseLineNumber :: Parser LineNumber
parseLineNumber = do
  a <- digit
  b <- digit
  c <- digit
  d <- digit

  return $ read $ a : b : c : d : []

parsePhone :: Parser PhoneNumber
parsePhone = skipCountryCode >>
  PhoneNumber <$> parseNumberingPlanArea <*> parseExchange <*> parseLineNumber

-------------------------

logFile = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

type Description = String

data Activity = Activity Time Description

instance Show Activity where
  show (Activity time description) = show time ++ " " ++ description

data DayLog = DayLog DateTime [Activity]

instance Show DayLog where
  show (DayLog datetime activities) =
    "# " ++ show datetime ++ "\n" ++
    intercalate "\n" (map show activities)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComment :: Parser ()
skipComment = many (char ' ') *> string "--" *> void (manyTill anyChar ((void newline) <|> eof))

parseDate' :: Parser DateTime
parseDate' = do
  string "# "
  year <- integer
  char '-'
  month <- integer
  char '-'
  day <- integer
  try skipComment <|> (do return ())
  skipEOL

  return $ DateTime (fromIntegral year) (fromIntegral month) (fromIntegral day) 0 0 0

parseActivity :: Parser Activity
parseActivity = do
  hour <- integer
  char ':'
  minute <- integer

  description <- manyTill anyChar (try skipComment <|> void newline <|> eof)

  return $ Activity (Time (fromIntegral hour) (fromIntegral minute) 0) description

parseDayLog :: Parser DayLog
parseDayLog =
  many (try skipComment <|> void newline) *>
    (DayLog <$> parseDate' <*> some parseActivity)

parseDayLogs :: Parser [DayLog]
parseDayLogs = some parseDayLog

-----------------

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

ipFormatError :: Parser a
ipFormatError = unexpected "wrong ip address format"

parseOctet :: Parser Word32
parseOctet = do
  num <- integer

  if num > 255
    then ipFormatError
    else do
      return $ fromIntegral num

parseIPV4 :: Parser IPAddress
parseIPV4 = do
  three <- parseOctet
  char '.'
  two <- parseOctet
  char '.'
  one <- parseOctet
  char '.'
  zero <- parseOctet

  let f x = 2 ^ (x * 8)

  return $ IPAddress (three * f 3 + two * f 2 + one * f 1 + zero * f 0)

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

