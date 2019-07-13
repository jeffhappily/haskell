module TypeClass where

-- Basic sum type declaration
data Bool = False | True

class MyEq a where
  eql :: a -> a -> Bool
  notEql :: a -> a -> Bool
  {-# MINIMAL eql | notEql #-}

-- Record syntax (Product type), fields can be accessed via function name same as field
-- Eg: 
--      let person = Person {firstName = "Jeff", lastName = "Cheah", age = 21}
--      firstName person   -- Jeff
--
data Person = Person 
    { firstName :: String
    , lastName :: String
    , age :: Int
    } deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Either a b = Left a | Right b
    
main = do
    let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
    putStrLn $ show mikeD
