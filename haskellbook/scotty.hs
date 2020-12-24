{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import           Control.Monad.IO.Class
import           Data.Monoid               (mconcat)
import           Web.Scotty

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $
      mconcat ["<h1>Scotty, ",
               beam,
               " me up!</h1>"]
