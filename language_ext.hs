{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module LanguageExt where

a = 1

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

type Nat f g = forall a . f a -> g a
