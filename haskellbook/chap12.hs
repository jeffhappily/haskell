module Chap12 where

-- import           Data.Maybe hiding (fromMaybe)
-- import           Prelude

vowels :: String
vowels = "aeiou"

-- Helper
isVowel :: Char -> Bool
isVowel c = elem c vowels

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe xs    = Just xs

replaceThe :: String -> String
replaceThe = unwords . map ((fromMaybe "a") . notThe) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = fst . foldl go (0, False) . words
  where
    go (n, _) "the"    = (n, True)
    go (n, True) (x:_) = if isVowel x then (n+1, False) else (n, False)
    go tuple  _        = tuple


countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (\x -> not $ isVowel x || x == ' ')

newtype Word' =
  Word' String
    deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs = case compare vw cs of
  GT -> Nothing
  _  -> Just $ Word' xs
  where
    vw = countVowels xs
    cs = countConsonants xs

data Nat
  = Zero
  | Succ Nat
    deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ foldr (const Succ) Zero [1..n]

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing  = x
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybee x id

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes []            = []
catMaybes ((Just a):xs) = a : catMaybes xs
catMaybes (_:xs)        = catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs = if length xs /= length ys then Nothing else Just ys
  where
    ys = catMaybes xs

lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' xs = foldr
  (\x z -> case x of
    Left a -> a : z
    _      -> z)
  [] xs

rights' :: [Either a b] -> [b]
rights' [] = []
rights' xs = foldr
  (\x z -> case x of
    Right a -> a : z
    _       -> z)
  [] xs

partitionEithers' :: [Either a b]
  -> ([a], [b])
partitionEithers' [] = ([], [])
partitionEithers' ls = foldr
  (\x (xs, yz) -> case x of
    Left a  -> (a:xs, yz)
    Right b -> (xs, b:yz))
  ([], []) ls

eitherMaybe' :: (b -> c)
  -> Either a b
  -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right a) = Just $ f a

either' :: (a -> c)
  -> (b -> c)
  -> Either a b
  -> c
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c)
  -> Either a b
  -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b))
  -> b
  -> [a]
myUnfoldr f b = case f b of
  Nothing      -> []
  Just (a, b') -> a : myUnfoldr f b'

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
  -> a
  -> BinaryTree b
unfold f a = case f a of
  Just (left, val, right) -> Node (unfold f left) val (unfold f right)
  Nothing                 -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\x -> if x >= n then Nothing else Just (x+1, x, x+1)) 0
