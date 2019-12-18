module RandomExample where

import           Control.Applicative       (liftA2, liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import qualified Data.DList                as DL
import           System.Random

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

-------------------

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

fizzbuzzFromTo :: Integer
  -> Integer
  -> [String]
fizzbuzzFromTo from to
  | from > to = []
  | otherwise = fizzBuzz from : fizzbuzzFromTo (from + 1) to

main :: IO ()
main =
  mapM_ putStrLn $ fizzbuzzList [1..100]
