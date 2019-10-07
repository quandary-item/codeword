{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Text.Printf

import BoardProblem
import Problem

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Solution = Map.Map Int Char

solve :: Problem -> Solution
solve = undefined

toFeatures :: String -> [(Int, Int, Char)]
toFeatures word = zip3 (repeat $ length word) [0..] (map toUpper word)

makeIndex :: (Ord a, Ord b) => (a -> [b]) -> [a] -> Map.Map b (Set.Set a)
makeIndex getKeys values = Map.fromListWith (<>) $ concatMap something values
  where something word = zip (getKeys word) (repeat $ Set.singleton word)

intersections :: (Ord a) => [Set.Set a] -> Set.Set a
intersections [] = Set.empty
intersections (x:xs) = foldl Set.intersection x xs

multipleLookup :: (Ord k, Ord v) => [k] -> Map.Map k (Set.Set v) -> Set.Set v
multipleLookup keys mapping = intersections $ map (flip (Map.findWithDefault Set.empty) mapping) keys

showSolution :: BoardProblem -> Solution -> String
showSolution (BoardProblem { rows = rows }) mapping = showGrid 2 lookupCell rows
  where
    -- replace the usual function used to display cells (show) with one that tries to look up the cell
    -- in the mapping
    lookupCell CellEmpty = show CellEmpty
    lookupCell (CellNumber i) = case (Map.lookup i mapping) of
      Just c -> [c]
      Nothing -> show i
