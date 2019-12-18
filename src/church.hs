zero :: (a -> a) -> a -> a
zero f x = x

one :: (a -> a) -> a -> a
one f x = f x

successor :: (a -> a) -> a -> a -> a -> a
successor n f x = f $ n f x
