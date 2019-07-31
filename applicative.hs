module Applicative where

import           Data.Monoid

main :: IO ()
main = do
  -- instance Monoid a => Applicative ((,) a)
  -- first type argument to two-tuple has to be a monoid
  print $ ("Woo", (+1)) <*> (" Hoo!", 0) -- ("Woo Hoo!",1)

  print $ (Sum 2, (+1)) <*> (Sum 9, 0) -- (Sum {getSum = 11},1)
