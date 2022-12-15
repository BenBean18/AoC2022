module Day15 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List
import Data.List.Split
import Data.Maybe
import Criterion.Main
import System.Environment

insert1D :: Int -> a -> [a] -> [a]
insert1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ zs

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

delete1D :: Int -> [a] -> [a]
delete1D i l = let (ys,zs) = splitAt i l in ys ++ (tail zs)

data Scanner = Scanner { location :: Point, beacon :: Point, dist :: Int } deriving (Eq, Show, Ord)
data Point = Point Int Int deriving (Eq, Show, Ord)

-- calculate entire grid
-- subtract the circles where beacons can't exist
-- only store numbers, not the whole thing
-- need area of slice of circle for part 1

-- yIsCloseEnough = (manhattan_distance - abs (y_of_scanner - 2000000)) > 0
-- centerX = x_of_scanner
-- leftX = centerX - (manhattan_distance - abs (y_of_scanner - 2000000))
-- rightX = centerX + (manhattan_distance - abs (y_of_scanner - 2000000))

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = (abs (x2-x1)) + (abs (y2-y1))

-- 5151312 is too low

parseScanner :: String -> Scanner
parseScanner s = 
    let matches = s =~ "Sensor at x=([-\\d]+), y=([-\\d]+): closest beacon is at x=([-\\d]+), y=([-\\d]+)" :: [[String]]
        location = Point (read ((head matches) !! 1) :: Int) (read ((head matches) !! 2) :: Int)
        beacon = Point (read ((head matches) !! 3) :: Int) (read ((head matches) !! 4) :: Int)
        dist = manhattanDistance location beacon in
            Scanner { location = location, beacon = beacon, dist = dist }

-- what happens if they don't overlap?

-- check if overlap
-- if they do then combine
-- otherwise leave the list unchanged
-- do that for every pair

doesOverlap :: (Int, Int) -> (Int, Int) -> Bool
doesOverlap (h1, t1) (h2, t2) = (h2 <= h1 && h1 <= t2) || (h1 <= h2 && h2 <= t1)

-- 1,2,3,4,5
-- 2,3,4,5,6
-- p1 = (1,3)
-- p2 = (2,6)

-- 1,2,3,4,5
-- 1,2,3,4
-- p1 = (1,5)
-- p2 = (1,4)

-- only works for overlapping
combineRanges :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineRanges (h1_, t1_) (h2_, t2_) =
    (min h1_ h2_, max t1_ t2_)

-- [r1,r2,r3,r4] -> [r1&2,r3,r4] || [r1,r2,r3,r4]
-- what if [r1,r2,r3,r4] [] -> [r3,r4] [r1,r2] || [r3,r4] [r1&2]

-- check overlap on head with each of tail
-- if overlaps then combine and restart

combineIfOverlaps :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
combineIfOverlaps p1 p2 =
    if doesOverlap p1 p2 then [p1, p2] else [combineRanges p1 p2]

combineListOfRanges' :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
combineListOfRanges' l idx1 idx2 =
    if idx1 == (length l-1) then l
    else
        let p1 = (l !! idx1)
            p2 = (l !! idx2) in
                if doesOverlap p1 p2 then
                    let newList = delete1D idx2 (set1D idx1 (combineRanges p1 p2) l) in combineListOfRanges' newList 0 1
                else if idx2 >= ((length l) - 1) then combineListOfRanges' l (idx1+1) 1
                else combineListOfRanges' l idx1 (idx2+1)

combineListOfRanges :: [(Int, Int)] -> [(Int, Int)]
combineListOfRanges l = combineListOfRanges' l 0 1

getRange :: Scanner -> Int -> [(Int, Int)]
getRange Scanner { location = (Point sx sy), dist = manDist, beacon = (Point bx by) } y = 
    if (manDist - abs (sy - y)) > 0 then
        let leftX = sx - (manDist - abs (sy - y))
            rightX = sx + (manDist - abs (sy - y)) in
                [(leftX, rightX)]
    else []

rangeSpan :: (Int, Int) -> Int
rangeSpan (x1, x2) = abs (x1-x2)

getRanges :: [Scanner] -> Int -> [(Int, Int)]
getRanges scanners y = combineListOfRanges (foldl (\current s -> (getRange s y) ++ current) [] scanners)

part1' lines = do
    let scanners = map parseScanner lines
    let ranges = getRanges scanners 2000000
    print $ ranges
    let total = foldl (\current r -> (rangeSpan r) + current) 0 ranges
    print $ total

-- Part 2

-- Need to store x and y overlap not just a specific row.
part2' lines = do
    let scanners = map parseScanner lines
    let ranges = filter (\(r, y) -> length r > 1) (zip [getRanges scanners y | y <- [0..4000000]] [0..4000000])
    if length ranges == 1 then
        let x = (snd $ head $ fst $ head ranges) + 1
            y = snd $ head ranges in
                print ((x * 4000000) + y)
    else print "No beacon location found"
    --let total = foldl (\current r -> (rangeSpan r) + current) 0 ranges
    --print $ total

-- Benchmarking
part1 = do
    lines <- getLines "day15/input.txt"
    part1' lines

part2 = do
    lines <- getLines "day15/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day15.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day15/input.txt"
    time lines