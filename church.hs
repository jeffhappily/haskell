zero :: (a -> a) -> a -> a
zero _f x = x

one :: (a -> a) -> a -> a
one f = f

successor :: ((a -> a) -> a -> a) -> (a -> a) -> a -> a
successor n f x = f $ n f x
