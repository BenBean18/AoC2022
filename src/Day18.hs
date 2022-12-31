module Day18 where

import Utilities
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Data.List (sort, group)
import Debug.Trace
import Criterion.Main
import System.Environment
-- 0 0 0 Up
-- 0 0 1 Down
data Point = Point Int Int Int deriving (Show, Ord, Eq)
data Face = Up | R | Forward | Down | L | Back deriving (Enum, Show, Eq, Bounded, Ord)
data Side = Side Point Face deriving (Show)
instance Eq Side where
    (Side p1 f1) == (Side p2 f2) = (go p1 f1 == p2 && f1 == oppositeFace f2) || (p1 == p2 && f1 == f2)
instance Ord Side where
    (Side p1 f1) `compare` (Side p2 f2) = if (Side p1 f1) == (Side p2 f2) then EQ else
        if p1 `compare` p2 == EQ then f1 `compare` f2 else p1 `compare` p2

nextFace :: Face -> Face
nextFace Back = Up
nextFace f = succ f

oppositeFace :: Face -> Face
oppositeFace f = nextFace $ nextFace $ nextFace f

go :: Point -> Face -> Point
go (Point x y z) Up = (Point x y (z+1))
go (Point x y z) Down = (Point x y (z-1))
go (Point x y z) L = (Point (x-1) y z)
go (Point x y z) R = (Point (x+1) y z)
go (Point x y z) Forward = (Point x (y+1) z)
go (Point x y z) Back = (Point x (y-1) z)

addCubes :: [Point] -> [Side] -> [Side]
addCubes pts sides = foldl (flip (:)) sides [Side p face | face <- [minBound..maxBound], p <- pts]

parseCube :: String -> Point
parseCube s = let [x,y,z] = map (\x -> read x :: Int) $ splitOn "," s in (Point x y z)

-- O(n^2) I think? Might be even worse...
-- This runs really slowly, I need to find a clever way to remove duplicates while adding
deleteAllDupeCopies :: (Ord a, Eq a) => [a] -> [a]
deleteAllDupeCopies (x:xs) = if x `elem` xs then deleteAllDupeCopies (filter ((/=) x) xs) else x:deleteAllDupeCopies xs
deleteAllDupeCopies [] = []

part1 = do
    lines <- getLines "day18/input.txt"
    let cubes = map parseCube lines
    let sides = addCubes cubes []
    let unique = deleteAllDupeCopies sides
    print $ length unique

-- Part 2

-- Need to find 8 cubes arranged in a box with none inside...
-- A side is on the outside if the cube that it `go`es to has no sides on it

-- note to self: need to exclude the face that is requesting
-- ... this still includes an inside chamber with multiple in it
-- maybe iterate through this 1000 times? should catch all chambers (hopefully)?
onOutside :: Side -> Int -> [Side] -> [Side] -> Bool
onOutside s 20 _ _ = True
onOutside (Side p f) iters sides visited =
    if (Side p f) `elem` sides && iters /= 0 then False else
    let pt = (go p f)
        neighbors = [Side pt face | face <- [minBound..maxBound]{-, not ((Side pt face) `elem` visited)-}{-, face /= oppositeFace f-}]
        unblocked = (filter (\n -> not (n `elem` sides) && not (n `elem` visited)) neighbors)
        blocked = (filter (\n -> (n `elem` sides)) neighbors)
        numUnblocked = length unblocked in
            if numUnblocked == 0 then False
            else if numUnblocked >= 6 then True -- only need to keep checking if not completely exposed
            else 
                let truths = map (\n -> onOutside n (iters+1) sides ((Side p f):visited)) neighbors in foldl (||) False truths

-- 4078 too high
-- 3992 too high
-- 3648 too high

-- need to determine if an entire chamber is bounded

-- flood :: [Side] -> [Side] -> Set.Set Side -> Set.Set Side
-- flood [] sides visited = visited
-- flood ((Side p f):toVisit) sides visited =
--     let neighbors = filter ((flip elem) sides) [Side (go p f) face | face <- [minBound..maxBound], face /= oppositeFace f] in
--         if not (foldl (||) True (map (\n -> n `notElem` visited) neighbors)) then visited -- nowhere to go
--         else flood (Set.toList $ Set.fromList $ toVisit ++ neighbors) sides (Set.insert (Side p f) visited)

-- there can be a line of empty space so can't just check if surrounded

processOutsides :: [Side] -> [Side] -> Map.Map Side Bool -> Int -> Int
processOutsides [] _ _ i = i
processOutsides (x:xs) sides m i = (traceShow $ length xs) (
    if x `Map.member` m then ($!) processOutsides xs sides m (i + (if m Map.! x then 1 else 0))
    else
        let outside = ($!) onOutside x 0 sides [] in
            ($!) processOutsides xs sides (Map.insert x outside m) (i + (if outside then 1 else 0)))

part2 = do
    lines <- getLines "day18/input.txt"
    let cubes = map parseCube lines
    let sides = addCubes cubes []
    let unique = deleteAllDupeCopies sides
    -- print $ onOutside (Side (Point 0 (-59) (-7)) Down) 0 unique []
    -- print $ onOutside (Side (Point 0 (-59) (-7)) Up) 0 unique []
    -- print $ onOutside (Side (Point 0 (-59) (-7)) L) 0 unique []
    -- print $ onOutside (Side (Point 0 (-59) (-7)) R) 0 unique []
    -- print $ onOutside (Side (Point 0 (-59) (-7)) Forward) 0 unique []
    -- print $ onOutside (Side (Point 0 (-59) (-7)) Back) 0 unique []
    print $ processOutsides unique unique Map.empty 0
    -- let out = foldl (+) 0 (map (\side -> if onOutside side 0 unique [] then 1 else 0) unique)
    -- print out