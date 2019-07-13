{-# LANGUAGE NoMonomorphismRestriction #-}

module Test where

a = 1

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")
