{-# LANGUAGE DeriveGeneric #-}

module Problem where

import GHC.Generics

-- internal representation of parsed problems
data CodeWord = CodeWord [Int] deriving Generic
instance Show CodeWord where
  show (CodeWord ints) = show ints
type Problem = [CodeWord]
