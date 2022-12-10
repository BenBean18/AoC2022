module Day4 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List
import qualified Data.Map as Map
import System.Environment
import Criterion.Main

-- input: e.g. "123-345,456-789"
-- output: ([123..345],[456..789])
parseLine :: String -> ([Int],[Int])
parseLine l = 
    let matches = l =~ "(\\d+)-(\\d+),(\\d+)-(\\d+)" :: [[String]] in
        if length matches == 1 && length (matches !! 0) == 5 then ([(read ((matches !! 0) !! 1) :: Int)..(read ((matches !! 0) !! 2) :: Int)], [(read ((matches !! 0) !! 3) :: Int)..(read ((matches !! 0) !! 4) :: Int)])
        else ([],[])

-- let's try some currying!
-- returns if r2 fits fully within r1
isBoundedBy :: [Int] -> [Int] -> Bool
isBoundedBy r1 r2 = head r1 <= head r2 && last r1 >= last r2

bidirectionalBoundedBy :: ([Int],[Int]) -> Int
bidirectionalBoundedBy (one, two) = boolToInt (isBoundedBy one two || isBoundedBy two one)

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

part1' lines = do
    print (sum (map bidirectionalBoundedBy (map parseLine lines)))

-- Part 2

-- if the head of one is completely contained within the other then they overlap
-- have to check both ways though
doesOverlap :: ([Int], [Int]) -> Bool
doesOverlap (r1, r2) = (head r2 <= head r1 && head r1 <= last r2) || (head r1 <= head r2 && head r2 <= last r1)

part2' lines = do
    print (sum (map boolToInt (map doesOverlap (map parseLine lines))))

-- Benchmarking

part1 = do
    lines <- getLines "day4/input.txt"
    part1' lines

part2 = do
    lines <- getLines "day4/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day4.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day4/input.txt"
    time lines


-- Part 2 Visualization maybe

findOverlapping :: ([Int], [Int]) -> [Int]
findOverlapping (r1, r2) = let l = sort [head r1, last r1, head r2, last r2] in
    if doesOverlap (r1, r2) then [l !! 1..l !! 2]
    else []

-- Input: Int
-- Output: (Int, 1)
addOne :: Int -> (Int, Int)
addOne i = (i, 1)

-- defaultJust :: Maybe Int -> Int
-- defaultJust (Just a) = a
-- defaultJust Nothing = 0

-- -- Input: list of overlapping pairs
-- -- Output: Map<value, occurrences>
-- getHeatmap :: ([([Int], [Int])], Map.Map Int Int) -> Map.Map Int Int
-- getHeatmap ([], m) = m
-- getHeatmap (l, m) = getHeatmap (drop 1 l, (Map.fromList (map (`addOne` (defaultJust (m !? ))) (findOverlapping (head l)))))

fromJust :: Maybe a -> a
fromJust Nothing  = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- Function getHeatmap. Input: list of overlapping pairs. Output: Map where the key is the overlapping value and the value is the number of times it overlaps.
-- This function should be optimized to minimize memory usage.
getHeatmap :: [([Int], [Int])] -> Map.Map Int Int
getHeatmap l = foldl (\m (k, v) -> Map.insertWith (+) k v m) Map.empty (map (\x -> (x, 1)) (concat (map findOverlapping l)))

-- getHeatmap :: [([Int], [Int])] -> Map.Map T Int
-- getHeatmap [] = Map.empty
-- getHeatmap list = let nextMap = getHeatmap (tail list) in
--     if nextMap !? (head list) == Nothing then insertWith (+) (findOverlapping (head list)) 1
--     else Map.empty

-- ok i tried...
-- https://stackoverflow.com/questions/7108559/how-to-find-the-frequency-of-characters-in-a-string-in-haskell
-- this is insanely unoptimized, it creates a giant list of all overlapping squares (20488 long for my input)
printFreqMap = do
    lines <- getLines "day4/input.txt"
    -- print (length ((concat (map (findOverlapping . parseLine) lines))))
    -- print (Map.toList $ Map.fromListWith (+) [(c, 1) | c <- (concat (map (findOverlapping . parseLine) lines))])
    print (getHeatmap (map parseLine lines))