{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Char (toUpper)
import Data.List (intercalate, transpose)
import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics
import Text.Printf

someFunc :: IO ()
someFunc = putStrLn "someFunc"

r' = [ [ cn 16, cn 14, cn 22, cn 19, cn 5 , cn 6 , cn 26, cE   , cn 25, cn 26, cn 16, cn 16, cn 19, cn 16, cn 16 ]
     , [ cn 22, cE   , cn 19, cE   , cn 19, cE   , cn 25, cE   , cn 22, cE   , cn 8 , cE   , cn 17, cE   , cn 13 ]
     , [ cn 10, cn 21, cn 17, cn 3 , cn 4 , cn 3 , cn 10, cn 5 , cn 11, cE   , cn 10, cn 7 , cn 3 , cn 19, cn 21 ]
     , [ cn 9 , cE   , cn 10, cE   , cn 11, cE   , cn 13, cE   , cn 16, cE   , cn 15, cE   , cn 8 , cE   , cn 3  ]
     , [ cn 25, cn 26, cn 9 , cn 25, cE   , cn 18, cn 21, cn 16, cn 3 , cn 20, cn 19, cn 16, cn 16, cn 19, cn 16 ]
     , [ cn 26, cE   , cn 19, cE   , cn 9 , cE   , cn 19, cE   , cn 14, cE   , cn 14, cE   , cE   , cE   , cn 22 ]
     , [ cn 26, cn 21, cn 8 , cn 4 , cn 19, cn 8 , cE   , cn 9 , cn 10, cn 12, cn 26, cn 5 , cn 3 , cn 8 , cn 11 ]
     , [ cE   , cE   , cn 19, cE   , cn 9 , cE   , cn 18, cE   , cn 4 , cE   , cn 10, cE   , cn 20, cE   , cE    ]
     , [ cn 22, cn 26, cn 5 , cn 9 , cn 26, cn 20, cn 10, cn 4 , cE   , cn 3 , cn 14, cn 19, cn 14, cn 10, cn 25 ]
     , [ cn 10, cE   , cE   , cE   , cn 5 , cE   , cn 16, cE   , cn 25, cE   , cn 22, cE   , cn 26, cE   , cn 5  ]
     , [ cn 4 , cn 26, cn 13, cn 21, cn 10, cn 14, cn 3 , cn 26, cn 21, cn 16, cE   , cn 26, cn 15, cn 4 , cn 19 ]
     , [ cn 23, cE   , cn 21, cE   , cn 20, cE   , cn 4 , cE   , cn 4 , cE   , cn 10, cE   , cn 20, cE   , cn 8  ]
     , [ cn 2 , cn 19, cn 3 , cn 5 , cn 7 , cE   , cn 3 , cn 9 , cn 25, cn 10, cn 16, cn 16, cn 3 , cn 1 , cn 19 ]
     , [ cn 10, cE   , cn 8 , cE   , cn 21, cE   , cn 16, cE   , cn 3 , cE   , cn 22, cE   , cn 8 , cE   , cn 20 ]
     , [ cn 11, cn 10, cn 16, cn 22, cn 9 , cn 10, cn 24, cE   , cn 8 , cn 22, cn 11, cn 5 , cn 26, cn 3 , cn 7  ]
     ]

testProblem = BoardProblem { rows = r'}


leftPad :: Char -> Int -> String -> String
leftPad c n s = prefix ++ s
  where
    prefixLength = max 0 $ n - (length s)
    prefix = replicate prefixLength c

leftPadSpace = leftPad ' '

showGrid :: Int -> (a -> String) -> [[a]] -> [Char]
showGrid cellWidth f = intercalate "\n" . map showLine
  where
    showLine = intercalate " " . map showCell
    showCell = leftPadSpace cellWidth . f

-- the language for specifying problems
cn = CellNumber
cE = CellEmpty

data Cell = CellEmpty | CellNumber Int deriving (Generic, Eq)
instance Show Cell where
  show CellEmpty = "XX"
  show (CellNumber i) = show i
  
data BoardProblem = BoardProblem { rows :: [[Cell]]} deriving (Generic)
instance Show BoardProblem where
  show (BoardProblem { rows = rows } ) = showGrid 2 show rows

-- internal representation of parsed problems
data CodeWord = CodeWord [Int] deriving Generic
instance Show CodeWord where
  show (CodeWord ints) = show ints
type Problem = [CodeWord]

splitEmptyCells = split (dropBlanks $ dropDelims $ oneOf [CellEmpty])

toNumber :: Cell -> Int
toNumber (CellNumber i) = i

notSingleton :: [a] -> Bool
notSingleton x = length x > 1

getRowCodeWords :: [Cell] -> Problem
getRowCodeWords cells = map toCodeWord $ filter notSingleton $ splitEmptyCells cells
  where toCodeWord = CodeWord . map toNumber

importProblem :: BoardProblem -> Problem
importProblem (BoardProblem { rows = rows }) = rowCodeWords ++ columnCodeWords
  where
    rowCodeWords = concatMap getRowCodeWords rows
    columnCodeWords = concatMap getRowCodeWords (transpose rows)


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
