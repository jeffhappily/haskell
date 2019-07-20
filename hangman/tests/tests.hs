module MainTest where

import           Data.Char
import           Data.Maybe      (isJust)
import qualified Data.Set        as Set
import           Hangman
import           Test.QuickCheck

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates list = length list /= length set
  where set = Set.fromList list

genAlphaChar :: Gen Char
genAlphaChar = arbitrary `suchThat` isAsciiLower

genPuzzle :: Gen Puzzle
genPuzzle = do
  a <- listOf genAlphaChar
  c <- listOf genAlphaChar `suchThat` (not . hasDuplicates)

  let b = map (\x -> if x `elem` c then Just x else Nothing) a

  return $ Puzzle a b c

prop_fillInCharacter :: Property
prop_fillInCharacter =
  forAll genPuzzle (\p@(Puzzle _ discovered guessed) ->
    forAll genAlphaChar (\c ->
      let
        Puzzle _ newDiscovered newGuessed = fillInCharacter p c
        cmpDiscoveredLength = compare (length $ filter isJust newDiscovered) (length $ filter isJust discovered)
        cmpGuessedLength = compare (length newGuessed) (length guessed) in
      case (charInWord p c, alreadyGuessed p c) of
        (False, _)   -> cmpDiscoveredLength == EQ && cmpGuessedLength == GT
        (True, True) -> cmpDiscoveredLength == EQ && cmpGuessedLength == GT
        (_, _)       -> cmpDiscoveredLength == GT && cmpGuessedLength == GT))

main :: IO ()
main = do
  quickCheck prop_fillInCharacter
