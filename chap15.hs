module Chap15 where

import           Data.Monoid

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> x        = x
  x <> Nada        = x
  Only x <> Only y = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madlibbinBetter' :: Exclamation
  -> Adverb
  -> Noun
  -> Adjective
  -> String
madlibbinBetter' e adv noun adj = mconcat
  [ e, "! he said "
  , adv , " as he jumped into his car "
  , noun , " and drove off with his "
  , adj , " wife."
  ]
