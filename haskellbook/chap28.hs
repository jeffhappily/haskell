module Chap28 where

import           Criterion.Main

-- Difference list
newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL a) = a []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL a) = DL ((x:) . a)
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc (DL a) x = DL (a . (x:))
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append (DL a) (DL b) = DL $ a . b
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs =
          go (n-1)
          (singleton n `append` xs)

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a =
  Queue 
    { enqueue :: [a]
    , dequeue :: [a]
    } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue e d) = Queue (x:e) (d)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] [])    = Nothing
pop (Queue e (x:xs)) = Just (x, Queue e xs)
pop (Queue e [])     = Just (x, Queue e xs)
  where
    (x:xs) = reverse e

main :: IO ()
main = defaultMain
  [ bench "concat list" $
    whnf schlemiel 123456
  , bench "concat dlist" $
    whnf constructDlist 123456
  ]
