module Day8 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Utilities

delFirstLastIndex :: Int -> [Int] -> [Int]
delFirstLastIndex maxI list =
    filter (\x -> (x /= 0) && (x /= maxI-1)) list

visibleIndices' :: (Ord a, Bounded a) => [a] -> a -> Int -> [Int] -> [Int]
visibleIndices' [] m i visible = delFirstLastIndex i visible
visibleIndices' list currentMax i visible =
    if currentMax == maxBound then delFirstLastIndex i visible -- we're counting the first item and we need to not do that
    else
        if head list > currentMax then visibleIndices' (tail list) (head list) (i+1) (i : visible)
        else visibleIndices' (tail list) currentMax (i+1) visible
    
visibleIndices :: [Char] -> [Int]
visibleIndices l = 
    visibleIndices' l '0' 0 [] ++ (map (\x -> (length l - 1) - x) (visibleIndices' (reverse l) '0' 0 []))

-- Find visible indices in row.
-- Map visible indices from x to (x, y) because we know the y.
-- Add to a Set containing (x, y).
-- Transpose array.
-- Find visible indices in column.
-- Map visible indices from y to (x, y) because we know the x.
-- Add to a Set containing (x, y).
-- Return the length of the set.

data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)

findRowIndices :: [Char] -> Int -> Set.Set Point
findRowIndices row y =
    Set.fromList $ map (\i -> Point { x = i, y = y }) (visibleIndices row)

findColIndices :: [Char] -> Int -> Set.Set Point
findColIndices col x =
    Set.fromList $ map (\i -> Point { x = x, y = i }) (visibleIndices col)

-- this expects no top & bottom
findAllRowIndices :: [[Char]] -> Int -> Set.Set Point -> Set.Set Point
findAllRowIndices [] index set = set
findAllRowIndices chars index set =
    let newSet = findRowIndices (head chars) index in
        findAllRowIndices (tail chars) (index+1) (Set.union set newSet)

-- this expects a transposed array with no left or right
findAllColIndices :: [[Char]] -> Int -> Set.Set Point -> Set.Set Point
findAllColIndices [] index set = set
findAllColIndices chars index set =
    let newSet = findColIndices (head chars) index in
        findAllColIndices (tail chars) (index+1) (Set.union set newSet)

part1 = do
    lines <- getLines "day8/input.txt"
    let rowSet = findAllRowIndices (tail (init lines)) 1 Set.empty
    let colSet = findAllColIndices (tail (init (List.transpose lines))) 1 Set.empty
    print $ (length (Set.union rowSet colSet)) + (length lines) * 2 + (length (lines !! 0)) * 2 - 4 -- -4 to account for corners