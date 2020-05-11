module Main where

import           Criterion.Main
import qualified Data.Map       as M
import qualified Data.Sequence  as Seq
import qualified Data.Set       as S
import qualified Data.Vector as V

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: Int -> M.Map Int Int
insertMap i = M.insert i i m

insertSet :: Int -> S.Set Int
insertSet i = S.insert i s

list :: [Int]
list = [1..100000]

seq' :: Seq.Seq Int
seq' = Seq.fromList [1..100000]

lists :: [[Int]]
lists = replicate 10 list

seqs :: [Seq.Seq Int]
seqs = replicate 10 seq'

slice :: Int -> Int -> [a] -> [a]
slice from len xs = take len (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

testV :: Int -> V.Vector Int
testV n =
  V.map ( (+n) . (+n)
  . (+n) . (+n) )
  (V.fromList [1..10000])

-- Equivalent to testV (loop fusion)
testV' :: Int -> V.Vector Int
testV' n =
  V.map (+n) $ V.map (+n) $
  V.map (+n) $ V.map (+n)
  (V.fromList [1..10000])

infixl 9 !?
{-# INLINABLE (!?) #-}

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      (\x r k ->
        case k of
          0 -> Just x
          _ -> r (k-1))
      (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

main :: IO ()
main =
  defaultMain
    [ bench "index list 9999"
      $ whnf (myList !!) 9998
    , bench "index list maybe index 9999"
      $ whnf (myList !?) 9998
    , bench "map list 9999"
      $ whnf (map (+1)) myList
    , bench "force map list 9999"
      $ nf (map (+1)) myList
    ,  bench "member check map"
      $ whnf membersMap 9999
    , bench "member check set"
      $ whnf membersSet 9999
    ,  bench "insert check map"
      $ whnf insertMap 10000
    , bench "insert check set"
      $ whnf insertSet 10000
    , bench "concatenate lists"
      $ nf mconcat lists
    , bench "concatenate sequences"
      $ nf mconcat seqs
    , bench "indexing list"
      $ whnf (\xs -> xs !! 9001) list
    , bench "indexing sequence"
      $ whnf (flip Seq.index 9001) seq'
    ,  bench "slicing list"
      $ whnf (head . slice 100 900) l
    , bench "slicing vector"
      $ whnf (V.head . V.slice 100 900) v
    , bench "vector map prefused"
      $ whnf testV 9998
    , bench "vector map will be fused"
      $ whnf testV' 9998
    ]
