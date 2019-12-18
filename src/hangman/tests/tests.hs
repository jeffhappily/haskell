module MainTest where

import           Data.Char
import           Data.Maybe      (isJust)
import qualified Data.Set        as Set
import           Hangman
import           Test.Hspec
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

handleGuessSpec :: IO ()
handleGuessSpec = hspec $ do
  describe "handleGuess" $ do
    context "Guessed character is in word" $ do
      it "guessed length increases" $ do
        let Puzzle _ newDiscovered newGuessed = fillInCharacter p 'e'

        compare (length newGuessed) (length guessed) `shouldBe` GT

      it "discovered length increases if char is not guessed" $ do
        let Puzzle _ newDiscovered newGuessed = fillInCharacter p 'e'

        compare
          (length $ filter isJust newDiscovered)
          (length $ filter isJust discovered)
          `shouldBe` GT

      it "discovered length remains if char is guessed" $ do
        let Puzzle _ newDiscovered newGuessed = fillInCharacter p 'h'

        compare
          (length $ filter isJust newDiscovered)
          (length $ filter isJust discovered)
          `shouldBe` EQ

    context "Char not in word" $ do
      it "guessed length increases" $ do
        let Puzzle _ newDiscovered newGuessed = fillInCharacter p 'x'

        compare (length newGuessed) (length guessed) `shouldBe` GT

      it "discovered length remains" $ do
        let Puzzle _ newDiscovered newGuessed = fillInCharacter p 'x'

        compare
          (length $ filter isJust newDiscovered)
          (length $ filter isJust discovered)
          `shouldBe` EQ

  where
    p@(Puzzle _ discovered guessed) = Puzzle "hello" [Just 'h', Nothing, Nothing, Nothing, Nothing] "zckih"

main :: IO ()
main = do
  quickCheck prop_fillInCharacter
  handleGuessSpec
