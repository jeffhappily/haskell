{-# LANGUAGE FlexibleContexts #-}

module Chap21 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Example on when to use Traversable
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- There's a decoder function that makes
-- some object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj]
  -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- after
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
  a <- fetchFn query

  traverse makeIoOnlyObj (traverse decodeFn a)

----------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

----------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-----------------

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada    = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [return Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-------------------

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ z Nil         = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
  -- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
  traverse _ Nil         = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-----------------

data Three a b c =
  Three a b c
  deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

--------------------

data Pair a b =
  Pair a b
  deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

--------------------

data Big a b =
  Big a b b
  deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

------------------

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

----------------

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n , Arbitrary (n a) , Arbitrary a ) => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance ( Applicative n , Testable (n Property) , Eq a , Eq (n a) , EqProp a) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S xs x) = S (fmap f xs) (f x)

instance Foldable n => Foldable (S n) where
  foldMap f (S xs x) = foldMap f xs <> f x

instance Traversable n => Traversable (S n) where
  traverse f (S xs x) = S <$> traverse f xs <*> f x

---------------

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node x a y) = Node (fmap f x) (f a) (fmap f y)

-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node x a y) = foldMap f x <> f a <> foldMap f x

  foldr _ z Empty        = z
  foldr f z (Leaf a)     = f a z
  foldr f z (Node x a y) = f a $ foldr f (foldr f z x) y

instance Traversable Tree where
  traverse _ Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Node x a y) = Node <$> traverse f x <*> f a <*> traverse f y

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [return Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = do
  let
    iden :: Identity (Int, Int, [Int])
    iden = undefined

    cons :: Constant (Int, Int, [Int]) (Int, Int, [Int])
    cons = undefined

    opt :: Optional (Int, Int, [Int])
    opt = undefined

    lst :: List (Int, Int, [Int])
    lst = undefined

    three :: Three Int Int (Int, Int, [Int])
    three = undefined

    pair :: Pair Int (Int, Int, [Int])
    pair = undefined

    big :: Big Int (Int, Int, [Int])
    big = undefined

    bigger :: Bigger Int (Int, Int, [Int])
    bigger = undefined

    s :: S [] (Int, Int, [Int])
    s = undefined

    tree :: Tree (Int, Int, [Int])
    tree = undefined

  quickBatch (traversable iden)
  quickBatch (traversable cons)
  quickBatch (traversable opt)
  quickBatch (traversable lst)
  quickBatch (traversable three)
  quickBatch (traversable pair)
  quickBatch (traversable big)
  quickBatch (traversable bigger)
  quickBatch (traversable s)
  quickBatch (traversable tree)
