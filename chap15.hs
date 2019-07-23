module Chap15 where

import           Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada   <> x      = x
  x      <> Nada   = x
  Only x <> Only y = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e
    <> "! he said "
    <> adv
    <> " as he jumped into his car "
    <> noun
    <> " and drove off with his "
    <> adj
    <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj = mconcat
  [ e
  , "! he said "
  , adv
  , " as he jumped into his car "
  , noun
  , " and drove off with his "
  , adj
  , " wife."
  ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull
  = Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary

    frequency [(1, return $ First' Nada), (1, return $ First' $ Only a)]

instance Semigroup a => Semigroup (First' a) where
  First' Nada <> x           = x
  x           <> First' Nada = x
  First' x    <> First' y    = First' $ x <> y

instance Semigroup a => Monoid (First' a) where
  mempty = First' Nada

firstMappend :: Semigroup a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  let ma  = monoidAssoc
      mli = monoidLeftIdentity
      mri = monoidRightIdentity

  quickCheck (ma :: BullMappend)
  quickCheck (mli :: Bull -> Bool)
  quickCheck (mri :: Bull -> Bool)
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
