{-# LANGUAGE InstanceSigs #-}

module RandomExample where

import           Control.Applicative       (liftA2, liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Checkers
-- import           Test.QuickCheck.Classes
-- import           Test.QuickCheck.Gen

-- Six-sided die
data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let
    s = mkStdGen 0
    (d1, s1) = randomR (1, 6) s
    (d2, s2) = randomR (1, 6) s1
    (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' =
  intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsCountLogged :: Int
  -> StdGen
  -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count acc gen
      | sum >= n = (count, acc)
      | otherwise =
        let
          (die, nextGen) = randomR (1, 6) gen
        in go (sum + die)
          (count + 1) (acc ++ [intToDie die]) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN = (fst .) . rollsCountLogged

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20

----------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Monoid s => Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (,) a

  (<*>) :: Moi s (a -> b)
    -> Moi s a
    -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let
        (h, s') = f s
        (a, s'') = g s in
      (h a, s' <> s'')

instance Monoid s => Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
    -> (a -> Moi s b)
    -> Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let
        (a, s') = f s
        (Moi h) = g a
        (b, s'') = h s in
      (b, s' <> s'')

-- instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Moi s a) where
--   arbitrary = Moi <$> arbitrary

-- instance (Eq s, Eq a, Arbitrary s) => EqProp (Moi s a) where
--   Moi f =-= Moi g = unGen $ liftA2 eq (f <$> x) (g <$> x)
--     where x = arbitrary

-- instance Show (Moi s a) where
--   show _ = "moi"

-- main :: IO ()
-- main = do
--   let
--     moi :: Moi String (String, String, [String])
--     moi = undefined

  -- quickBatch (applicative moi)
