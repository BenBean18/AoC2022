module Day4 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE

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

part1 = do
    lines <- getLines "day4/input.txt"
    print (sum (map bidirectionalBoundedBy (map parseLine lines)))

-- Part 2

-- if the head of one is completely contained within the other then they overlap
-- have to check both ways though
doesOverlap :: ([Int], [Int]) -> Bool
doesOverlap (r1, r2) = (head r2 <= head r1 && head r1 <= last r2) || (head r1 <= head r2 && head r2 <= last r1)

part2 = do
    lines <- getLines "day4/input.txt"
    print (sum (map boolToInt (map doesOverlap (map parseLine lines))))