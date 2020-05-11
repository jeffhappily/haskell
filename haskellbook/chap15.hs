module Chap15 where

import           Data.Monoid
import           Test.QuickCheck hiding (Failure, Success)

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

-------------------

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

---------------

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

----------------

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

----------------

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

-------------

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial


semigroupAssoc :: (Eq m, Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary

    return $ Identity a

type IdenAssoc =
  Identity String -> Identity String -> Identity String -> Bool

--------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    return $ Two a b

type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

--------------

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary

    return $ Three a b c

type ThreeAssoc = Three String [Int] String -> Three String [Int] String -> Three String [Int] String -> Bool

--------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary

    return $ Four a b c d

type FourAssoc = Four String [Int] String [Int] -> Four String [Int] String [Int] -> Four String [Int] String [Int] -> Bool

---------------

newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj x <> BoolConj y = BoolConj $ x && y

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary

    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj x <> BoolDisj y = BoolDisj $ x || y

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary

    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

----------------

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  _ <> x     = x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    oneof [return $ Fst a, return $ Snd b]

type OrAssoc = Or String [Int] -> Or String [Int] -> Or String [Int] -> Bool

-----------------

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
  show _ = "Combine <function>"

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g
  -- Equivalent as
  -- Combine f <> Combine g = Combine $ \x ->
  --   (f x) <> (g x)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

prop_combineAssoc :: Property
prop_combineAssoc =
  forAll (arbitrary :: Gen (Combine String String, Combine String String, Combine String String))
  (\(f, g, h) ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unCombine ((f <> g) <> h) xs == unCombine (f <> (g <> h)) xs))

prop_combineLeftIdentity :: Property
prop_combineLeftIdentity =
  forAll (arbitrary :: Gen (Combine String String))
  (\f ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unCombine (mempty <> f) xs == unCombine f xs))

prop_combineRightIdentity :: Property
prop_combineRightIdentity =
  forAll (arbitrary :: Gen (Combine String String))
  (\f ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unCombine (f <> mempty) xs == unCombine f xs))



--------------

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Show (Comp a) where
  show _ = "Comp <function>"

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance Semigroup a => Monoid (Comp a) where
  mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

prop_compAssoc :: Property
prop_compAssoc =
  forAll (arbitrary :: Gen (Comp String, Comp String, Comp String))
  (\(f, g, h) ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unComp ((f <> g) <> h) xs == unComp (f <> (g <> h)) xs))

prop_compLeftIdentity :: Property
prop_compLeftIdentity =
  forAll (arbitrary :: Gen (Comp String))
  (\f ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unComp (mempty <> f) xs == unComp f xs))

prop_compRightIdentity :: Property
prop_compRightIdentity =
  forAll (arbitrary :: Gen (Comp String))
  (\f ->
    forAll (arbitrary :: Gen String)
    (\xs ->
      unComp (f <> mempty) xs == unComp f xs))


data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure x <> Failure y = Failure $ x <> y
  Success x <> _ = Success x
  _ <> Success x = Success x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    oneof [return $ Failure a, return $ Success b]

type ValidAssoc = Validation String [Int] -> Validation String [Int] -> Validation String [Int] -> Bool

---------------

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ 
    \n -> let
      (x, y) = f n
      (x', s) = g y in
    (x <> x', s)
        
instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \x -> (mempty, x)

main :: IO ()
main = do
  -- let ma  = monoidAssoc
  --     mli = monoidLeftIdentity
  --     mri = monoidRightIdentity

  quickCheck (monoidAssoc :: BullMappend)
  -- Not valid identity
  -- quickCheck (monoidLeftIdentity :: Bull -> Bool)
  -- quickCheck (monoidRightIdentity :: Bull -> Bool)
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdenAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two String [Int] -> Bool)

  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck prop_combineAssoc
  quickCheck prop_combineLeftIdentity
  quickCheck prop_combineRightIdentity

  quickCheck prop_compAssoc
  quickCheck prop_compLeftIdentity
  quickCheck prop_compRightIdentity

  quickCheck (semigroupAssoc :: ValidAssoc)

  let f' = Mem $ \s -> ("hi", s + 1)
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
