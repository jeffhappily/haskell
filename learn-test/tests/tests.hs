module Main where

import           Data.List       (sort)
import           Test.QuickCheck

-- for a function
half :: Double -> Double
half x = x / 2

-- this property should hold
halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Property
prop_halfIdentity =
  forAll (arbitrary :: Gen Double)
  (\n -> halfIdentity n == n)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, _)       = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered =
  forAll (arbitrary :: Gen [Int])
  (\xs -> listOrdered (sort xs))

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

prop_plusAssociative :: Property
prop_plusAssociative =
  forAll (arbitrary :: Gen (Int, Int, Int))
  (\(x, y, z) -> plusAssociative x y z)

prop_plusCommutative :: Property
prop_plusCommutative =
  forAll (arbitrary :: Gen (Int, Int))
  (\(x, y) -> plusCommutative x y)

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative :: Int -> Int -> Bool
multCommutative x y =
  x * y == y * x

prop_multAssociative :: Property
prop_multAssociative =
  forAll (arbitrary :: Gen (Int, Int, Int))
  (\(x, y, z) -> multAssociative x y z)

prop_multCommutative :: Property
prop_multCommutative =
  forAll (arbitrary :: Gen (Int, Int))
  (\(x, y) -> multCommutative x y)

-- quot rem
quotRem' :: Int -> Int -> Bool
quotRem' x y = (quot x y) * y + (rem x y) == x

divMod' :: Int -> Int -> Bool
divMod' x y = (div x y) * y + (mod x y) == x

genDividentAndDivisor :: Gen (Int, Int)
genDividentAndDivisor = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= 0)

  return (a, b)

prop_quotRem' :: Property
prop_quotRem' =
  forAll genDividentAndDivisor
  (\(x, y) -> quotRem' x y)

prop_divMod' :: Property
prop_divMod' =
  forAll genDividentAndDivisor
  (\(x, y) -> divMod' x y)

expoAssociative :: Int -> Int -> Int -> Bool
expoAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

expoCommutative :: Int -> Int -> Bool
expoCommutative x y =
  x ^ y == y ^ x

prop_expoAssociative :: Property
prop_expoAssociative =
  forAll (arbitrary :: Gen (Int, Int, Int))
  (\(x, y, z) -> expoAssociative x y z)

prop_expoCommutative :: Property
prop_expoCommutative =
  forAll (arbitrary :: Gen (Int, Int))
  (\(x, y) -> expoCommutative x y)

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative
  quickCheck prop_quotRem'
  quickCheck prop_divMod'

  -- Not associative nor commutative
  -- quickCheck prop_expoAssociative
  -- quickCheck prop_expoCommutative
