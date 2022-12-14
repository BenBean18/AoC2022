{-# LANGUAGE BangPatterns #-}
module Day14 where

import Utilities
import Data.List.Split
import Text.Regex.Base
import Text.Regex.PCRE
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
-- parse into 2d array
-- ideally find a way to get final state without running all of the simulation steps
-- (I assume part 2 will be more compute intensive)

-- Sparse 2D array (map? set?)
-- Or just a regular one? That would be memory inefficient though

-- divvy 2 1 $ map (\x -> Day14.Point (read (x !! 2) :: Int) (read (x !! 3) :: Int)) ("498,4 -> 498,6 -> 496,6" =~ "((\\d+),(\\d+))" :: [[String]])

-- 5..3 no work

ptsInString :: String -> [Point]
ptsInString s =
    let divvied = divvy 2 1 $ map (\x -> Point (read (x !! 2) :: Int) (read (x !! 3) :: Int)) (s =~ "((\\d+),(\\d+))" :: [[String]])
        mapped = map ptsInList divvied in
            concat mapped

data Point = Point Int Int deriving (Show, Eq, Ord)
data MapState = Air | Sand | Rock deriving (Enum, Show, Eq)
type SandMap = Map.Map Point MapState

ptsInList :: [Point] -> [Point]
ptsInList [a, b] = ptsInLine a b

ptsInLine :: Point -> Point -> [Point]
ptsInLine (Point x1_ y1_) (Point x2_ y2_) =
    let x1 = min x1_ x2_
        x2 = max x1_ x2_
        y1 = min y1_ y2_
        y2 = max y1_ y2_ in -- this works because lines are straight not diagonal
    [(Point x y) | x <- [x1..x2],
                   y <- [y1..y2]]

addRockToMap :: SandMap -> Point -> SandMap
addRockToMap m p = Map.insert p Rock m

addRocksToMap :: SandMap -> [Point] -> SandMap
addRocksToMap m [] = m
addRocksToMap m rocks = let newMap = addRockToMap m (head rocks) in addRocksToMap newMap (tail rocks)

addLineToMap :: SandMap -> String -> SandMap
addLineToMap m s = addRocksToMap m (ptsInString s)

isFree :: SandMap -> Point -> Bool
-- use ?! has to be Just Air or Nothing
isFree m p = 
    let maybeValue = m Map.!? p in
        if isNothing maybeValue then True else fromJust maybeValue == Air

calcMaxY :: SandMap -> Int
calcMaxY m = maximum (map (\(Point _ y) -> y) (Map.keys m))

moveSand :: SandMap -> Point -> Int -> (SandMap, Bool)
moveSand m (Point x y) maxY =
    if y < maxY then (
        if isFree m (Point x (y+1)) then moveSand m (Point x (y+1)) maxY
        else if isFree m (Point (x-1) (y+1)) then moveSand m (Point (x-1) (y+1)) maxY
        else if isFree m (Point (x+1) (y+1)) then moveSand m (Point (x+1) (y+1)) maxY
        else (Map.insert (Point x y) Sand m, False))
    else (m, True)

countSand :: SandMap -> Int
countSand m = length $ filter (== Sand) (Map.elems m)

allSand :: SandMap -> Point -> SandMap
allSand m p = let !maxY = calcMaxY m
                  (newMap, end) = moveSand m p maxY in
                    if end then newMap else allSand newMap p

part1 = do
    lines <- getLines "day14/input.txt"
    let sm = foldl addLineToMap Map.empty lines
    let finalMap = allSand sm (Point 500 0)
    print $ countSand finalMap

-- Part 2

isFree2 :: SandMap -> Point -> Int -> Bool
-- use ?! has to be Just Air or Nothing
isFree2 m (Point x y) maxY =
    let maybeValue = m Map.!? (Point x y) in
        if y >= maxY + 2 then False
        else if isNothing maybeValue then True
        else fromJust maybeValue == Air

-- bug:
-- maxY is continually being increased, leading to a recursive loop.
-- make sure maxY is only calculated once
-- fix: line 117

moveSand2 :: SandMap -> Point -> Int -> (SandMap, Bool)
moveSand2 m (Point x y) maxY =
    if (Map.notMember (Point 500 0) m) then (
        if isFree2 m (Point x (y+1)) maxY then moveSand2 m (Point x (y+1)) maxY
        else if isFree2 m (Point (x-1) (y+1)) maxY then moveSand2 m (Point (x-1) (y+1)) maxY
        else if isFree2 m (Point (x+1) (y+1)) maxY then moveSand2 m (Point (x+1) (y+1)) maxY
        else (Map.insert (Point x y) Sand m, False))
    else (m, True)

allSand2 :: SandMap -> Point -> Int -> SandMap
allSand2 m p maxY = let (newMap, end) = moveSand2 m p maxY in
                    if end then newMap else allSand2 newMap p maxY

part2 = do
    lines <- getLines "day14/input.txt"
    let sm = foldl addLineToMap Map.empty lines
    let !maxY = calcMaxY sm
    let finalMap = allSand2 sm (Point 500 0) maxY
    print $ countSand finalMap