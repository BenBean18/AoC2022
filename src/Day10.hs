module Day10 where

import Utilities
import qualified Data.Map as Map
import Text.Regex.Base
import Text.Regex.PCRE

-- A CPU!
-- I'm just going to build a map of cycle to register value in case it loops.
-- arguments: lines, current cycle, current map
-- output: new map
parseInput' :: [String] -> Int -> Map.Map Int Int -> Map.Map Int Int
parseInput' [] cycle m = m
parseInput' lines cycle m =
    let matchAddx = (head lines) =~ "addx ([\\d-]+)" :: [[String]] in
        if (length matchAddx) > 0 then
            let newMap = Map.insert (cycle + 1) (m Map.! cycle) (Map.insert (cycle + 2) ((m Map.! cycle) + (read ((matchAddx !! 0) !! 1) :: Int)) m) in
                parseInput' (tail lines) (cycle+2) newMap
        else
            let newMap = Map.insert (cycle + 1) (m Map.! cycle) m in
                parseInput' (tail lines) (cycle+1) newMap

-- off by one error, didn't read carefully enough
-- "The CPU has a single register, X, which starts with the value 1."
-- and another OBOE since there's not a zeroth cycle
parseInput :: [String] -> Map.Map Int Int
parseInput lines = parseInput' lines 1 (Map.fromList [(1, 1)])

getSignalStrength :: Map.Map Int Int -> Int -> Int
getSignalStrength m cycle =
    (m Map.! cycle)*cycle

part1 = do
    lines <- getLines "day10/input.txt"
    let states = parseInput lines
    print $ getSignalStrength states 20 + getSignalStrength states 60 + getSignalStrength states 100 + getSignalStrength states 140 + getSignalStrength states 180 + getSignalStrength states 220

-- Part 2

getCRTLine :: Map.Map Int Int -> Int -> Int -> String -> String
getCRTLine m 40 rowNumber currentLine = currentLine
getCRTLine m pixel rowNumber currentLine = 
    let currentSpriteMid = (m Map.! ((rowNumber*40)+(pixel+1))) in
        if (abs $ currentSpriteMid-pixel) <= 1 then getCRTLine m (pixel+1) rowNumber (currentLine ++ "#")
        else getCRTLine m (pixel+1) rowNumber (currentLine ++ " ")

printCRT' :: Map.Map Int Int -> Int -> String -> String
printCRT' m 6 currentCRT = currentCRT
printCRT' m rowNumber currentCRT =
    printCRT' m (rowNumber+1) (currentCRT ++ (getCRTLine m 0 rowNumber "") ++ "\n")

printCRT :: Map.Map Int Int -> String
printCRT m = printCRT' m 0 ""

part2 = do
    lines <- getLines "day10/input.txt"
    let states = parseInput lines
    putStr $ printCRT states