module Day16 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List.Split
import Data.PSQueue
import qualified Data.Set as Set
import Debug.Trace
-- This seems like a special case of pathfinding
-- Store the current path (all of it)
-- Valid moves: move to a different valve or open the current one
-- Use a priority queue to prioritize the path with the highest pressure
-- Increase pressure based on current valves open
-- Once all are open in a path, that's the winner
-- When you open a valve, add (30 - # moves) * pressure to the current path

data Valve = Valve { name :: String, neighbors :: [String], flowRate :: Int } deriving (Eq, Show, Ord)
data Move = Open Valve | MoveTo Valve deriving (Eq, Show, Ord)
data Path = Path [Move] [Valve] Int deriving (Eq, Show) -- moves, valves open, total pressure
instance Ord Path where
    (Path _ _ p1) `compare` (Path _ _ p2) = p1 `compare` p2

movesFor :: Valve -> [Valve] -> [Valve] -> [Move]
movesFor valve allValves openValves = 
    let moves = map (\v -> MoveTo v) (getNeighbors valve allValves) in
        if flowRate valve /= 0 && not (elem valve openValves) then (Open valve) : moves else moves

addMoveToPath :: Move -> Path -> Path
addMoveToPath (Open v) (Path moves openValves pressure) = Path (moves ++ [(Open v)]) (v : openValves) (pressure + ((30 - (length moves))*(flowRate v)))
addMoveToPath (MoveTo v) (Path moves openValves pressure) = Path (moves ++ [(MoveTo v)]) openValves pressure

invalved :: Move -> Valve
invalved (Open v) = v
invalved (MoveTo v) = v

pathsFor :: Path -> [Valve] -> [Path]
pathsFor (Path moves openValves pressure) allValves = map (\m -> addMoveToPath m (Path moves openValves pressure)) (movesFor (invalved (last moves)) allValves openValves)

initPath :: Valve -> [Valve] -> [Path]
initPath v allValves = map (\m -> addMoveToPath m (Path [] [] 0)) (movesFor v allValves [])

-- use record syntax next time
getMoves :: Path -> [Move]
getMoves (Path moves _ _) = moves

getValves :: Path -> [Valve]
getValves (Path _ valves _) = valves

getPressure :: Path -> Int
getPressure (Path _ _ p) = p

-- 1. Find prospective path with highest pressure (init = [Path [MoveTo neighbor1ofAA] [] 0...Path [MoveTo neighborNofAA] [] 0])
-- 2. If all valves are open, return the path
-- 3. Else, add other paths to pq and recurse

doStuff' :: [Path] -> Set.Set Path -> [Valve] -> Path
doStuff' paths alreadyVisited allValves =
    let p = head paths in
        if (length $ getValves p) == length (filter (\v -> flowRate v /= 0) allValves) then p
        else if length (getMoves p) >= 30 then doStuff' (filter (/= p) paths) (Set.insert p alreadyVisited) allValves
        else
            doStuff' ((filter (/= p) paths) ++ (filter (\x -> not (x `elem` alreadyVisited)) (pathsFor p allValves))) (Set.insert p alreadyVisited) allValves

doStuff :: [Valve] -> Path
doStuff allValves = doStuff' (initPath (head (filter (\v -> name v == "AA") allValves)) allValves) Set.empty allValves

-- parse valve
-- Valve EG has flow rate=21; tunnels lead to valves WZ, OF, ZP, QD
parseValve :: String -> Valve
parseValve s = 
    let matches = s =~ "Valve ([A-Z]+) has flow rate=(\\d+); tunnels* leads* to valves* ((?:[A-Z](?:, )*)+)" :: [[String]] in
        Valve { name = (head matches) !! 1, flowRate = read ((head matches) !! 2) :: Int, neighbors = splitOn ", " ((head matches) !! 3) }

getNeighbors :: Valve -> [Valve] -> [Valve]
getNeighbors valve allValves = filter (\Valve { name = n } -> n `elem` (neighbors valve)) allValves

parseValves :: [String] -> [Valve]
parseValves lines = map parseValve lines

part1 = do
    lines <- getLines "day16/input.txt"
    let valves = parseValves lines
    let bestPath = doStuff valves
    print $ bestPath

-- Alternate strategy:
-- Pathfind back from highest to current
-- If openable valve along the way that would provide more pressure than just moving on then open it

-- Alternate:
-- Get paths to all openable and find total pressure. Do the one with the highest. Repeat.

{-
opened in order:
DD (20) - 1 minute
BB (13) - 2 minutes
JJ (21) - 3 minutes
HH (22) - 7 minutes
EE (3)  - 3 minutes
CC (2)  - 1 minute
-}

-- So... find all paths going to unopened?

-- Find the entire tree of paths with each goal being unopened.

-- Starting from DD (28 min left I believe)
-- 2 minutes to get to BB, 1 to open = (28 - 2 - 1) * 13 = 325
-- I assume this lowers movement cost to another so it's basically killing two birds with one stone
-- It does, it lowers cost to JJ. How to account for that though?
-- 4 minutes to get to HH, 1 to open = (28 - 4 - 1) * 22 = 506
-- 3 minutes to get to JJ, 1 to open = (28 - 3 - 1) * 21 = 504
-- (others don't matter b/c so low)

    -- Going to HH, 23 min left
    -- 7 to get to JJ, 1 to open = (23 - 7 - 1) * 21 = 315
    -- 6 to get to BB, 1 to open = (23 - 6 - 1) * 13 = 208
    -- others don't matter b/c super low

        -- Going to JJ, 15 min left
        -- 3 to get to BB, 1 to open = (15 - 3 - 1) * 13 = 143. (ignoring from DD) to HH to JJ to BB = 506 + 315 + 143 = 964
        -- Others don't matter

        -- Going to BB, 16 min left
        -- 3 to get to JJ, 1 to open = (15 - 3 - 1) * 21 = 231. (ignoring from DD) to HH to BB to JJ = 506+208+231 = 945
    
    -- Going to BB, 25 min left
    -- 3 to get to JJ, 1 to open = (25 - 3 - 1) * 21 = 441
    -- 6 to get to HH, 1 to open = (25 - 6 - 1) * 22 = 396

        -- Going to JJ, 21 min left
        -- 7 to get to HH, 1 to open = (21 - 7 - 1) * 22 = 286 (ignoring from DD) to BB to JJ to HH = 325 + 441 + 286 = 1052 *

        -- Going to HH, 18 min left
        -- 7 to get to JJ, 1 to open = (18 - 7 - 1) * 21 = 210 (ignoring from DD) to BB to HH to JJ = 325+396+210 = 931

-- Star = highest which happens to be the puzzle answer

-- SO SHOULDN'T WE CHOOSE HH?????
-- I guess if you play out the paths all the way, it takes a ton of energy to double back
-- So maybe do this "go to highest" thing for all?

-- It's a tree
-- Find highest leaf