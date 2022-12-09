module Day8 where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Utilities
import Criterion.Main
import System.Environment

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

part1_ lines = do
    let rowSet = findAllRowIndices (tail (init lines)) 1 Set.empty
    let colSet = findAllColIndices (tail (init (List.transpose lines))) 1 Set.empty
    print $ (length (Set.union rowSet colSet)) + (length lines) * 2 + (length (lines !! 0)) * 2 - 4 -- -4 to account for corners

-- Part 2

-- To get the distance before blockage:
-- let list = [1,2,3,2,5]
-- Left:
--  check index 2, for example
--  let b = (reverse (take (n+1) list)) == [3,2,1]
--  

-- max == head list
-- pass tail list to maximumDescent'
maximumDescent' :: (Ord a) => [a] -> Int -> a -> Int
maximumDescent' [] count max = count
maximumDescent' list count max =
    if head list >= max then count + 1 -- last tree
    else maximumDescent' (tail list) (count + 1) max

maximumDescent :: (Ord a) => [a] -> Int
maximumDescent list = maximumDescent' (tail list) 0 (head list)

getLeftViewDist :: (Ord a) => [a] -> Int -> Int
getLeftViewDist list index = maximumDescent (reverse (take (index + 1) list))

getRightViewDist :: (Ord a) => [a] -> Int -> Int
getRightViewDist list index = maximumDescent (drop index list)

-- left right can be used for down up

getScenicScore :: (Ord a) => [[a]] -> [[a]] -> Point -> Int
getScenicScore list tlist pt =
    let lvd = getLeftViewDist (list !! (y pt)) (x pt)
        rvd = getRightViewDist (list !! (y pt)) (x pt)
        tvd = getLeftViewDist (tlist !! (x pt)) (y pt)
        bvd = getRightViewDist (tlist !! (x pt)) (y pt) in
            lvd * rvd * tvd * bvd

findMaxScenicScoreAlongX :: (Ord a) => [[a]] -> [[a]] -> Int -> Int -> Int -> Int
findMaxScenicScoreAlongX list tlist x y maxValue =
    if x == (length (list !! y) - 1) then maxValue
    else findMaxScenicScoreAlongX list tlist (x+1) y (max maxValue (getScenicScore list tlist (Point { x = x, y = y })))

findMaxScenicScore' :: (Ord a) => [[a]] -> [[a]] -> Int -> Int -> Int
findMaxScenicScore' list tlist y maxValue =
    if y == (length list - 1) then maxValue
    else findMaxScenicScore' list tlist (y+1) (max maxValue (findMaxScenicScoreAlongX list tlist 0 y 0))

findMaxScenicScore :: (Ord a) => [[a]] -> Int
findMaxScenicScore list = findMaxScenicScore' list (List.transpose list) 0 0

part2_ lines = do
    print $ findMaxScenicScore lines

-- Benchmarking/function definitions

part1 = do
    lines <- getLines "day8/input.txt"
    part1_ lines

part2 = do
    lines <- getLines "day8/input.txt"
    part2_ lines

time lines =
    withArgs ["--output", "day8.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1_ lines
      , bench "part2" $ nfIO $ part2_ lines
    ]

benchmark = do
    lines <- getLines "day8/input.txt"
    time lines