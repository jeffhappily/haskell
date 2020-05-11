{-# LANGUAGE InstanceSigs #-}

module Chap23 where

-- import           Control.Monad.Trans.State
-- import           Test.QuickCheck
-- import           Test.QuickCheck.Checkers
-- import           Test.QuickCheck.Classes
-- import           Test.QuickCheck.Gen

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Monoid s => Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ (,) a

  (<*>) :: State s (a -> b)
    -> State s a
    -> State s b
  (State f) <*> (State g) =
    State $ \s ->
      let
        (h, s') = f s
        (a, s'') = g s in
      (h a, s' <> s'')

-- instance Monoid s => Monad (State s) where
--   return = pure

--   (>>=) :: State s a
--     -> (a -> State s b)
--     -> State s b
--   (State f) >>= g =
--     State $ \s ->
--       let
--         (a, s') = f s
--         (State h) = g a
--         (b, s'') = h s in
--       (b, s' <> s'')

-- Saw this new implementation, not sure which one is correct
instance Monoid s => Monad (State s) where
  return = pure

  (>>=) :: State s a
    -> (a -> State s b)
    -> State s b
  (State f) >>= g =
    State $ \s ->
      let
        (a, s') = f s
        (State h) = g a in
      h s'
----------------------

-- instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (State s a) where
--   arbitrary = State <$> arbitrary

-- instance (Eq s, Eq a, Arbitrary s) => EqProp (State s a) where
--   State f =-= State g = unGen $ liftA2 eq (f <$> x) (g <$> x)
--     where x = arbitrary

-- instance Show (State s a) where
--   show _ = "State"

-- main :: IO ()
-- main = do
--   let
--     State :: State String (String, String, [String])
--     State = undefined

  -- quickBatch (applicative State)

-----------------

get :: State s s
get = State $ \x -> (x, x)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) = snd . sa

eval :: State s a -> s -> a
eval (State sa) = fst . sa

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
