import Data.Time

data DatabaseItem 
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate 
    (UTCTime
      (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (x:xs) = case x of
  DbDate time -> time : filterDbDate xs
  _ -> filterDbDate xs

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (x:xs) = case x of
  DbNumber n -> n : filterDbNumber xs
  _ -> filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = minimum (filterDbDate xs)

sumDb :: [DatabaseItem] -> Integer
sumDb xs = sum (filterDbNumber xs)

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral $ total `div` len
  where
    total = sumDb xs
    len   = toInteger $ length $ filterDbNumber xs


