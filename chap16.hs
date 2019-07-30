module Chap16 where

import           Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorComposeInt :: (Eq (f a), Functor f, Num a) => f a -> Bool
functorComposeInt = functorCompose (+ 1) (* 2)

------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  -- fmap Identity to Gen a, which returns Gen (Identity a)
  arbitrary = Identity <$> arbitrary

------------

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    return $ Pair a b

------------

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary

    return $ Two a b

------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary

    return $ Three a b c

------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary

    return $ Three' a b c

------------

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c $ f d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary

    return $ Four a b c d

------------

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c $ f d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary

    return $ Four' a b c d

------------

-- Identical to Maybe
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope     = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance Arbitrary a => Arbitrary (Possibly a) where
  arbitrary = oneof [return LolNope, Yeppers <$> arbitrary]

-- Identical to Either
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First  a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorComposeInt :: Identity Int -> Bool)

  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorComposeInt :: Pair Int -> Bool)

  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorComposeInt :: Two String Int -> Bool)

  quickCheck (functorIdentity :: Three [Int] String Int -> Bool)
  quickCheck (functorComposeInt :: Three [Int] String Int -> Bool)

  quickCheck (functorIdentity :: Three' [Int] Int -> Bool)
  quickCheck (functorComposeInt :: Three' [Int] Int -> Bool)

  quickCheck (functorIdentity :: Four Bool String [Int] Int -> Bool)
  quickCheck (functorComposeInt :: Four Bool String [Int] Int -> Bool)

  quickCheck (functorIdentity :: Four' Bool Int -> Bool)
  quickCheck (functorComposeInt :: Four' Bool Int -> Bool)

  quickCheck (functorIdentity :: Possibly Int -> Bool)
  quickCheck (functorComposeInt :: Possibly Int -> Bool)

  quickCheck (functorIdentity :: Sum String Int -> Bool)
  quickCheck (functorComposeInt :: Sum String Int -> Bool)
