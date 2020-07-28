{- stack script
    --resolver lts-16.6
    --install-ghc
    --ghc-options -Wall
    --package singletons
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import Data.Singletons.TH

-- Taken from Logical Foundation https://softwarefoundations.cis.upenn.edu/lf-current/Imp.html

$( singletons [d|
    data AExp
    data BExp
    data Com
      = CSkip
      | CBreak
      -- Not sure what error is this
      -- Couldn't match type ‘Demote Char’ with ‘Char’
      --   Expected type: String
      --   Actual type: Demote [Char]
      -- | CAss String AExp
      | CSeq Com Com
      | CIf BExp Com Com
      | CWhile BExp Com
    data State
    data Result
      = SContinue
      | SBreak
    |]
 )

data CEval :: Com -> State -> Result -> State -> Type where
  ESkip :: CEval CSkip s SContinue s
  EBreak :: CEval CBreak s SBreak s