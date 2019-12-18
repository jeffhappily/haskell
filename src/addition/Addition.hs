module Addition where

import           Test.Hspec
import           Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise =
        go (n - d) d (count + 1)

mult :: (Eq a, Num a, Ord a) => a -> a -> a
mult a b = go a b 0
  where
    go a' b' total
      | b' == 0 = total
      | otherwise =
        go a' (b' - 1) (total + a')

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = frequency [(1, return 1), (4, return 2), (1, return 3)]
-- equivalent as [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
  => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b,
  Arbitrary c)
  => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "x + 1 is always\
      \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

  describe "Multiplication" $ do
    it "2 mult 0 is 0" $ do
      mult 2 0 `shouldBe` 0
    it "22 mult 5 is 110" $ do
      mult 22 5 `shouldBe` 110


prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
