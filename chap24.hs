module Chap24 where

import           Control.Applicative
import           Data.Ratio              ((%))
import           Text.Parser.Combinators
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

  -- try (char '-') <|> skipMany (oneOf "\n")

  return $ SemVer major minor patch release metadata

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

