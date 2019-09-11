module Chap24 where

import           Control.Applicative
import           Data.Ratio              ((%))
import           Text.Parser.Combinators
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

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata

parseSemVer :: Parser SemVer
parseSemVer = undefined

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

