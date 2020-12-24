{-# LANGUAGE ExistentialQuantification #-}

data Showable = forall a. Show a => Showable a

a :: [Showable]
a =
  [ Showable (Just 1)
  , Showable (Just True)
  , Showable "Hello"
  ]
  
f :: Showable -> String
f (Showable a) = show a

main :: IO ()
main = do
  let xs = fmap f a
  print xs
