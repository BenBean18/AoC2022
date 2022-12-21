module Day16_part2 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List.Split
import qualified Data.PSQueue as PSQ
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Data.List (maximumBy)
import Data.Maybe
import Criterion.Main
import System.Environment
-- This seems like a special case of pathfinding
-- Store the current path (all of it)
-- Valid moves: move to a different valve or open the current one
-- Use a priority queue to prioritize the path with the highest pressure
-- Increase pressure based on current valves open
-- Once all are open in a path, that's the winner
-- When you open a valve, add (30 - # moves) * pressure to the current path

data Valve = Valve { name :: String, neighbors :: [String], flowRate :: Int } deriving (Show, Ord)
instance Eq Valve where
    Valve { name = n1 } == Valve { name = n2 } = n1 == n2
data Move = Open Valve | MoveTo Valve deriving (Eq, Show, Ord)
data Path = Path [Move] [Valve] Int deriving (Eq, Show) -- moves, valves open, total pressure
instance Ord Path where
    (Path _ _ p1) `compare` (Path _ _ p2) = p1 `compare` p2
compValve Valve { flowRate = r1 } Valve { flowRate = r2 } = r1 `compare` r2

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
    let p = maximum paths in (trace $ show $ p) (
        if (length $ getValves p) == length (filter (\v -> flowRate v /= 0) allValves) then p
        else if length (getMoves p) >= 30 then doStuff' (filter (/= p) paths) (Set.insert p alreadyVisited) allValves
        else
            doStuff' ((filter (/= p) paths) ++ (filter (\x -> not (x `elem` alreadyVisited)) (pathsFor p allValves))) (Set.insert p alreadyVisited) allValves)

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

-- part1 = do
--     lines <- getLines "day16/input.txt"
--     let valves = parseValves lines
--     let bestPath = doStuff valves
--     print $ bestPath

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

-- but.. there are ~1.3 terapaths for my input
-- maybe do BFS where the nodes are just the nonzero ones
-- cost = how many minutes used to move there & open
-- or cost = added pressure when going to that node, when reached end then yay, store how many minutes already used
-- A*? heuristic (to maximize) = current pressure?

-- Store movement cost to each one

bfs' :: [Valve] -> [Valve] -> Map.Map Valve Valve -> Map.Map Valve Valve
bfs' [] allValves cameFrom = cameFrom
bfs' frontier_ allValves cameFrom =
    let current = last frontier_
        frontier = init frontier_
        newValves = (filter (\v -> not (v `elem` (Map.keys cameFrom))) (getNeighbors current allValves)) in
            bfs' (newValves ++ frontier) allValves (cameFrom `Map.union` (Map.fromList [(x, current) | x <- newValves]))

bfs :: [Valve] -> Map.Map Valve Valve
bfs (v:vs) = bfs' [v] (v:vs) (Map.singleton v v)

path :: Valve -> Valve -> [Valve] -> [Valve] -> [Valve]
path end start current valves = let m = bfs' [start] valves (Map.singleton start start) in
    if end == start then current else
        let next = m Map.! end in
            if next == start then current ++ [next]
            else path next start (current ++ [next]) valves

allWorking :: [Valve] -> [Valve]
allWorking valves = filter (\valve -> flowRate valve /= 0) valves

cost :: Valve -> Valve -> [Valve] -> Int
cost from to m = length $ path from to [] m

foldingThingy :: Map.Map Valve [(Valve, Int)] -> (Valve, (Valve, Int)) -> Map.Map Valve [(Valve, Int)]
foldingThingy m (k, v) = Map.insertWith (++) k [v] m

findAA :: [Valve] -> Valve
findAA allValves = (head (filter (\v -> name v == "AA") allValves))

workingGraph :: [Valve] -> Map.Map Valve [(Valve, Int)]
workingGraph valves = let m = bfs valves in
    foldl foldingThingy Map.empty [(a, (b, cost a b valves)) | a <- findAA valves : allWorking valves, b <- allWorking valves]

-- Is this a DFS?

-- AA to JJ = 2
-- AA to HH = 5
-- AA to EE = 2
-- AA to DD = 1
-- AA to CC = 2
-- AA to BB = 1

-- Goal: path with the highest pressure

-- Ok so we have two things, one we want to minimize (minutes), one we want to maximize (pressure)
-- What if we do
data Cost = Cost { minutes :: Int, pressure :: Int } deriving (Eq, Show)
instance Ord Cost where
    Cost { minutes = m1, pressure = p1 } `compare` Cost { minutes = m2, pressure = p2 } = 
        let comp = p2 `compare` p1 in
            if comp == EQ then m1 `compare` m2 {- inverted b/c we want pressure but not minutes -} else comp
-- optimizing for lowest cost
-- and run Dijkstra's algorithm with the cost being a Cost.
-- once all valves opened, then return pressure cost

-- bfs2' :: PSQ.PSQ  -> [Valve] -> Map.Map Valve Valve -> Map.Map Valve Valve
-- bfs2' [] allValves cameFrom = cameFrom
-- bfs2' frontier_ allValves cameFrom =
--     let current = last frontier_
--         frontier = init frontier_
--         newValves = (filter (\v -> not (v `elem` (Map.keys cameFrom))) (getNeighbors current allValves)) in
--             bfs2' (newValves ++ frontier) allValves (cameFrom `Map.union` (Map.fromList [(x, current) | x <- newValves]))

-- D, 28 min left
-- (add one to open)
-- 3 min to J
-- 5 min to H
-- 1 min to E
-- 0 min to D
-- 1 min to C
-- 2 min to B

findPressure :: (Valve, Int) -> Int -> Int
findPressure (v, minsToGetThere) minsLeft = (flowRate v) * (minsLeft-minsToGetThere-1)

potentialPressure :: Valve -> Map.Map Valve [(Valve, Int)] -> [Valve] -> Int -> Int
potentialPressure v m opened mins =
    let neighbors = (filter (\(a, b) -> not (a `elem` opened)) (m Map.! v))
        pressures = map (\(a,b) -> findPressure (a,b) mins) neighbors in
            (foldl (+) 0 pressures) + (foldl (+) 0 (map (\v -> (flowRate v) * mins) opened))

valve :: String -> [Valve] -> Valve
valve s valves = head $ filter (\v -> name v == s) valves

-- this finds the lowest because it values potential (unachievable) pressure over actual pressure gained
-- so the lowest = highest potential

potentialPressures :: Valve -> Map.Map Valve [(Valve, Int)] -> [Valve] -> Int -> [(Valve, Int, Int)]
potentialPressures v m opened mins =
    let neighbors = (filter (\(a, b) -> not (a `elem` opened)) (m Map.! v))
        states = map (\(a,b) -> (a, mins - b - 1)) neighbors
        pps = map (\(a, b) -> (a, (potentialPressure a m (a:opened) b) + ((flowRate a) * (mins - b - 1)), b)) states in pps

-- check next level before pruning
-- 

-- frontier = Queue()
-- frontier.put(start )
-- came_from = dict() # path A->B is stored as came_from[B] == A
-- came_from[start] = None

-- while not frontier.empty():
--    current = frontier.get()
--    for next in graph.neighbors(current):
--       if next not in came_from:
--          frontier.put(next)
--          came_from[next] = current

bfsWorking :: [Valve] -> Map.Map Valve Valve -> Map.Map Valve [(Valve, Int)] -> Map.Map Valve Valve
bfsWorking [] cameFrom graph = cameFrom
bfsWorking frontier cameFrom graph = (trace $ show frontier) (
    let current = last frontier
        neighbors = graph Map.! current
        newNeighbors = {-filter (\(x, _) -> not (x `elem` (Map.keys cameFrom))) -}neighbors in
            bfsWorking ((map fst newNeighbors) ++ (init frontier)) (Map.union cameFrom (Map.fromList [((fst v), current) | v <- newNeighbors])) graph)

-- Dijkstra
-- frontier = PriorityQueue()
-- frontier.put(start, 0)
-- came_from = dict()
-- cost_so_far = dict()
-- came_from[start] = None
-- cost_so_far[start] = 0

-- while not frontier.empty():
--    current = frontier.get()

--    if current == goal:
--       break
   
--    for next in graph.neighbors(current):
--       new_cost = cost_so_far[current] + graph.cost(current, next)
--       if next not in cost_so_far or new_cost < cost_so_far[next]:
--          cost_so_far[next] = new_cost
--          priority = new_cost
--          frontier.put(next, priority)
--          came_from[next] = current
-- dijkstra :: PSQ.PSQ 

highestUnopened :: Valve -> Map.Map Valve [(Valve, Int)] -> [Valve] -> (Valve, Int)
highestUnopened v m opened = let neighbors = (filter (\(a, b) -> not (a `elem` opened)) (m Map.! v)) in
    maximumBy (\x1 x2 -> compValve (fst x1) (fst x2)) neighbors

-- function that returns the pressure going from highest to lowest
getOrderedPressure :: [Valve] -> Map.Map Valve [(Valve, Int)] -> Int -> Int -> Int
getOrderedPressure opened m minsLeft pressure =
    if length opened == length (Map.keys m) then pressure
    else
        let (highest,minutes) = highestUnopened (last opened) m opened
            newMins = minsLeft-minutes-1
            fr = flowRate highest
            p = fr * newMins in{- (trace $ show highest ++ " " ++ show newMins) -}
        getOrderedPressure (opened ++ [highest]) m newMins (pressure + p)

addedPossiblePressure :: [Valve] -> Map.Map Valve [(Valve, Int)] -> Int -> Int -> Int
addedPossiblePressure opened m minsLeft pressure =
    if length opened == length (Map.keys m) then pressure
    else
        let (highest,minutes) = highestUnopened (last opened) m opened
            newMins = minsLeft-minutes-1
            fr = flowRate highest
            p = fr * newMins in{- (trace $ show highest ++ " " ++ show newMins) -}
        addedPossiblePressure (opened ++ [highest]) m minsLeft (pressure + p)

addedPossiblePressure' :: NewPath -> Map.Map Valve [(Valve, Int)] -> Int
addedPossiblePressure' path m =
    let aNeigh = m Map.! (posA path)
        bNeigh = m Map.! (posB path)
        aMinsLeft = (minutesLeftA path)
        bMinsLeft = (minutesLeftB path)
        opened = (valves path)
        pressure = (p path) in
        foldl (+) pressure (map (\(valve, cost) -> (flowRate valve) * (aMinsLeft - cost - 1)) (filter (\(v,_) -> not (v `elem` opened)) aNeigh))
        +
        foldl (+) 0 (map (\(valve, cost) -> (flowRate valve) * (bMinsLeft - cost - 1)) (filter (\(v,_) -> not (v `elem` opened)) bNeigh))

-- BFS
-- Store current path
-- If the current pressure + (for all neighbors: (distance from last in current path * flow rate)) > stored maximum for current number of minutes,
    -- this is `addedPossiblePressure valves graph minutesLeft p`
-- then add neighbors to frontier. otherwise don't.
-- this is only on the current graph
-- it's actually Dijkstra because it's a priority queue<path, 100000000 - pressure> (big number minus because we have to find the minimum)
-- return once 30 moves or all are open

-- valves are [oldest, older, ..., newer, newest]
data NewPath = NewPath { valves :: [Valve], posA :: Valve, posB :: Valve, minutesLeftA :: Int, minutesLeftB :: Int, p :: Int, maxP :: Int } deriving (Show, Ord)
instance Eq NewPath where
    NewPath { valves = v1, posA = p1a, posB = p1b, minutesLeftA = m1a, minutesLeftB = m1b, p = p1 } == NewPath { valves = v2, posA = p2a, posB = p2b, minutesLeftA = m2a, minutesLeftB = m2b, p = p2 } = v1 == v2 && m1a == m2a && m1b == m2b && p1 == p2 && p1a == p2a && p1b == p2b
-- or do we need a custom instance to sort by priority?

genPath :: NewPath -> Valve -> Int -> Valve -> Int -> Int -> NewPath
genPath path a cA b cB mp =
    NewPath { valves = (valves path) ++ (if name a /= "" then [a] else []) ++ (if name b /= "" then [b] else []),
              posA = if name a /= "" then a else posA path,
              posB = if name b /= "" then b else posB path,
              minutesLeftA = (minutesLeftA path) - cA - (if name a /= "" then 1 else 0),
              minutesLeftB = (minutesLeftB path) - cB - (if name b /= "" then 1 else 0),
              p = (p path) + 
                  ((flowRate a) * ((minutesLeftA path) - cA - (if name a /= "" then 1 else 0))) + 
                  ((flowRate b) * ((minutesLeftB path) - cB - (if name b /= "" then 1 else 0))),
              maxP = mp
            }

neighborPaths :: NewPath -> Map.Map Valve [(Valve, Int)] -> Int -> [NewPath]
neighborPaths path m mp =
    let aPos = posA path
        bPos = posB path
        aNeighbors = m Map.! aPos
        bNeighbors = m Map.! bPos
        --paths = filter (\p -> minutesLeft p >= 0) $ map (\(valve, cost) -> NewPath { valves = (valves path) ++ [valve], minutesLeft = (minutesLeft path) - cost - 1, p = p path + (flowRate valve) * ((minutesLeft path) - cost - 1) }) (filter (\(valve, _) -> not (valve `elem` (valves path))) neighbors)
        -- need to account for when A doesn't move or B doesn't move
        paths = filter (\p -> minutesLeftA p >= 0 && minutesLeftB p >= 0) $ [genPath path a cA b cB mp | (a, cA) <- (Valve { name = "", flowRate = 0 }, 0):aNeighbors, (b, cB) <- (Valve { name = "", flowRate = 0 }, 0):bNeighbors, a /= b, not $ a `elem` (valves path), not $ b `elem` (valves path)]
            in paths
-- TODO need to return visited with the new paths added
-- edit: no. please don't do that. it means they can't be visited

-- 2141 is too low

--                 path   pressure maxP atMins
bfsGood :: PSQ.PSQ NewPath Int -> (Int, Int, Int) -> Map.Map Valve [(Valve, Int)] -> Int -> NewPath
bfsGood frontier_ (maxPressure, aMaxMins, bMaxMins) graph iters =
    let mv = PSQ.minView frontier_ in
        if isNothing mv then NewPath { valves = [], posA = Valve {name="",flowRate=0,neighbors=[]}, posB = Valve {name="",flowRate=0,neighbors=[]}, minutesLeftA = 0, minutesLeftB = 0, p = maxPressure, maxP = maxPressure }
        -- What other exit conditions are there?
        else
            let Just (currentBinding, frontier) = mv
                currentPath = PSQ.key currentBinding
                currentPressure = PSQ.prio currentBinding
                nextPressure = (addedPossiblePressure' currentPath graph)
                lastValve = last $ valves currentPath in
                    if (length $ valves currentPath) >= (length $ Map.keys graph) then currentPath -- all open
                    -- if we've exhausted the frontier of older paths
                    else if iters == 0 && maxPressure /= 0 && (length $ (filter (\path -> maxP (PSQ.key path) < maxPressure) $ PSQ.toList frontier)) < (length $ (filter (\path -> maxP (PSQ.key path) == maxPressure) $ PSQ.toList frontier)) then 
                        NewPath { valves = [], posA = Valve {name="",flowRate=0,neighbors=[]}, posB = Valve {name="",flowRate=0,neighbors=[]}, minutesLeftA = 0, minutesLeftB = 0, p = maxPressure, maxP = maxPressure }
                    else
                        if {-((minutesLeftA currentPath) > aMaxMins && (minutesLeftB currentPath) > bMaxMins) || -}nextPressure > maxPressure || (p currentPath) > maxPressure then
                            let nps = (neighborPaths currentPath graph maxPressure) in
                                if (p currentPath) > maxPressure then (trace $ show (p currentPath))
                                    -- so oddly, it only returns the correct solution if the priority is 100000000 - the pressure of the *current path*, not the one being examined/neighbor.
                                    -- I actually didn't mean to do that but for some reason it worked.
                                    -- So, is there a way to optimize based on that?
                                    -- i guess this optimizes based on largest pressure *difference*
                                    -- or it prioritizes old paths (lower maximum) so we don't stay at the end of unproductive trees?
                                    bfsGood (foldl (\q path -> PSQ.insert path (100000000-(p path)+maxPressure) q) frontier nps) (p currentPath, (minutesLeftA currentPath), (minutesLeftB currentPath)) graph ((iters + 1) `mod` 100000)
                                else bfsGood (foldl (\q path -> PSQ.insert path (100000000-(p path)+maxPressure) q) frontier nps) (maxPressure, aMaxMins, bMaxMins) graph ((iters + 1) `mod` 100000) -- the old way marked all next paths as visited so we wouldn't explore them
                        else bfsGood frontier (maxPressure, aMaxMins, bMaxMins) graph ((iters + 1) `mod` 100000)
            
                -- set max pressure
                -- do the thing on 353
    -- pop from queue, 

part2' lines = do
    let valves = ($!) parseValves lines
    let g = ($!) workingGraph valves
    let aa = valve "AA" valves
    print $ bfsGood (PSQ.singleton (NewPath { valves = [aa], posA = aa, posB = aa, minutesLeftA = 26, minutesLeftB = 26, p = 0, maxP = 0 }) 0) (0, 26, 26) g 0

-- Benchmarking

part2 = do
    lines <- getLines "day16/input.txt"
    part2' lines