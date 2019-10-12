{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Char (toUpper)
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Text.Printf

import BoardProblem
import Problem
import TreeSearch

someFunc :: IO ()
someFunc = putStrLn "someFunc"

allLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

type Solution = Map.Map Int Char
type CharKey = (Int, Int, Char)
type WordKey = [CharKey]
type WordIndex = Map.Map CharKey (Set.Set String)

solve :: Problem -> Solution
solve = undefined

lookupCodeWord :: Solution -> CodeWord -> WordKey
lookupCodeWord solution (CodeWord charValues) = toFeatures chars
  where chars = catMaybes $ map (flip Map.lookup solution) $ charValues

isComplete :: Solution -> Bool
isComplete solution = all (flip Map.member $ solution) [1..26]

existsInIndex :: WordIndex -> WordKey -> Bool
existsInIndex _ [] = True
existsInIndex wordIndex wordKey = not $ Set.null $ multipleLookup wordKey wordIndex

isInvalidSolution :: WordIndex -> Problem -> Solution -> Bool
isInvalidSolution wordIndex codewords solution = any (not . existsInIndex wordIndex) wordKeys
  where
    wordKeys = map (lookupCodeWord solution) codewords

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:xs) = Just x

nextNumber :: Solution -> Maybe Int
nextNumber solution = firstOrNothing $ filter (flip Map.notMember solution) [1..26]

toValues :: Map.Map k v -> [v]
toValues = (map snd) . Map.toList

availableChars :: Solution -> [Char]
availableChars solution = [x | x <- allLetters, not $ x `Set.member` existingLetters]
  where existingLetters = Set.fromList $ toValues solution

makeBranches :: Solution -> [Solution]
makeBranches solution = case nextNumber solution of
  Nothing -> []
  Just i  -> map (\char -> Map.insert i char solution) $ availableChars solution

solveCodeWordStep :: WordIndex -> Problem -> Solution -> Result Solution
solveCodeWordStep wordIndex problem solution
  | isInvalidSolution wordIndex problem solution = NoSolution
  | isComplete solution                          = Solution solution
  | otherwise                                    = Branches $ makeBranches solution


toFeatures :: String -> [(Int, Int, Char)]
toFeatures word = zip3 (repeat $ length word) [0..] (map toUpper word)

makeIndex :: (Ord a, Ord b) => (a -> [b]) -> [a] -> Map.Map b (Set.Set a)
makeIndex getKeys values = Map.map Set.fromList $ Map.fromListWith (<>) $ concatMap something values
  where something word = zip (getKeys word) (repeat $ [word])

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
