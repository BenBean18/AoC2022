module Day19 where

import qualified Data.Map as Map
import Text.Regex.Base
import Text.Regex.PCRE
import Utilities
import Data.List.Split
import Data.List (uncons, unzip)
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import qualified Data.PSQueue as PSQ

-- idea: work backwards from obsidian-collecting robot
-- most geodes = most obsidian-collecting robots

-- BFS probably
-- Next step is collect and optionally make a robot

-- Blueprint 1:
--   Each ore robot costs 4 ore.
--   Each clay robot costs 2 ore.
--   Each obsidian robot costs 3 ore and 14 clay.
--   Each geode robot costs 2 ore and 7 obsidian.

-- Blueprint 2:
--   Each ore robot costs 2 ore.
--   Each clay robot costs 3 ore.
--   Each obsidian robot costs 3 ore and 8 clay.
--   Each geode robot costs 3 ore and 12 obsidian.

data Mineral = Ore | Clay | Obsidian | Geode deriving (Ord, Show, Enum, Eq)
data Robot = Robot Mineral deriving (Ord, Show, Eq)
type Cost = Map.Map Mineral Int
type ResourceCount = Map.Map Mineral Int
type Blueprint = Map.Map Robot Cost
data State = State { robots :: [Robot], resources :: ResourceCount, minutes :: Int } deriving (Eq, Show, Ord)

mineral :: Robot -> Mineral
mineral (Robot m) = m

subtractResources :: ResourceCount -> ResourceCount -> ResourceCount
-- subtractResources r1 r2 = map (\k -> (r1 Map.! k) - (r2 Map.! k)) (Map.keys r1)
subtractResources r1 r2 = Map.unionWith (-) r1 r2

addResources :: ResourceCount -> ResourceCount -> ResourceCount
-- addResources r1 r2 = map (\k -> (r1 Map.! k) + (r2 Map.! k)) (Map.keys r1)
addResources r1 r2 = Map.unionWith (+) r1 r2

-- newState :: State -> Robot -> Blueprint -> State
-- newState State { robots = rs, resources = rc } r bp = State { robots = r:rs, resources = rc `subtractResources` (bp Map.! r) }

availableRobots :: ResourceCount -> Blueprint -> [Robot] -> [Robot]
availableRobots rc bp rs = filter (\robot -> (length $ filter (< 0) (Map.elems $ subtractResources rc (bp Map.! robot))) == 0) rs

mineralsToRC :: [Mineral] -> ResourceCount
mineralsToRC ms = Map.fromListWith (+) (map (\x -> (x, 1)) ms)

neighbors :: State -> Blueprint -> [State]
neighbors State { minutes = 24 } _ = [] -- likely needs to be changed for part 2
neighbors State { robots = rs, resources = rc, minutes = mins } bp = 
    let newResources = addResources rc $ mineralsToRC $ map mineral rs
        newRobotPossibilities = availableRobots newResources bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode] in
            -- 1. find robots which can be created (mineral count > mineral need)
            -- 2. get states with those robots
            -- there is also the state where just collection happens
            State { robots = rs, resources = newResources, minutes = mins + 1 } : map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities -- ugh, need to subtract resources from newResources for every robot...

stringToMineral :: String -> Mineral
stringToMineral "ore" = Ore
stringToMineral "geode" = Geode
stringToMineral "clay" = Clay
stringToMineral "obsidian" = Obsidian

parseMineralAndCount :: [String] -> (Mineral, Int)
parseMineralAndCount [_, num, mineral] = (stringToMineral mineral, read num :: Int)

parseLine :: String -> Cost
parseLine l = 
    let matches = l =~ "(\\d+) ([^ .]+)" :: [[String]] in Map.fromList (map parseMineralAndCount matches)

parseBlueprint :: [String] -> Blueprint -> Blueprint
parseBlueprint [] blueprint = blueprint
-- ls length = 3 then ore
--    length = 2 then clay
--    length = 1 then obsidian
--    length = 0 then geode
parseBlueprint (l:ls) blueprint =
    let len = length ls
        val = parseLine l in
        if len == 3 then parseBlueprint ls (Map.insert (Robot Ore) val blueprint)
        else if len == 2 then parseBlueprint ls (Map.insert (Robot Clay) val blueprint)
        else if len == 1 then parseBlueprint ls (Map.insert (Robot Obsidian) val blueprint)
        else if len == 0 then parseBlueprint ls (Map.insert (Robot Geode) val blueprint)
        else error "the heck?"
-- oops...blueprints look like "Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 20 clay. Each geode robot costs 2 ore and 12 obsidian."

emptyRC :: ResourceCount
emptyRC = Map.fromList [(Ore, 0), (Clay, 0), (Obsidian, 0), (Geode, 0)]

nonOre :: State -> Blueprint -> [State]
nonOre State { minutes = 24 } _ = [] -- likely needs to be changed for part 2
nonOre State { robots = rs, resources = rc, minutes = mins } bp = 
    let newResources = addResources rc $ mineralsToRC $ map mineral rs
        newRobotPossibilities = availableRobots newResources bp [Robot Clay, Robot Obsidian, Robot Geode] in
            -- 1. find robots which can be created (mineral count > mineral need)
            -- 2. get states with those robots
            -- there is also the state where just collection happens
            State { robots = rs, resources = newResources, minutes = mins + 1 } : map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities -- ugh, need to subtract resources from newResources for every robot...

-- what if prioritized by ore equivalent?

priority :: State -> Int
priority State { robots = rs, minutes = mins, resources = rc } = 10000 - 
    (
        (length $ filter (== (Robot Ore)) rs) +
        ((length $ (filter (== (Robot Clay)) rs)) * 2) +
        ((length $ filter (== (Robot Obsidian)) rs) * 3) +
        ((length $ filter (== (Robot Geode)) rs) * 100)) + mins + rc Map.! Geode

-- Not sure what the exit condition should be. Maybe # of iterations with that maximum?

-- bfs blueprint frontier visited maximumGeodes -> maximumGeodes for "tree branches" below
bfs :: Blueprint -> PSQ.PSQ State Int -> Set.Set State -> Int -> Int
bfs bp frontier_ visited maximumGeodes =
    if PSQ.null frontier_ then maximumGeodes
    else
        let Just (state_, frontier) = PSQ.minView frontier_
            state = PSQ.key state_
            neighs = filter (`notElem` visited) $ neighbors state bp
            neighPriorities = zip neighs (map priority neighs)
            newMax = maximum (maximumGeodes:(map (\s -> (Map.!) (resources s) Geode) neighs)) in (trace $ show (minutes state) ++ " " ++ show newMax ++ " " ++ show maximumGeodes) (
                if (minutes state) == 24 && (resources state) Map.! Geode /= 0 then newMax
                else max newMax $ bfs bp (foldl (\q (neigh, prio) -> PSQ.insert neigh prio q) frontier neighPriorities) (foldl (flip Set.insert) visited neighs) newMax)

-- now it stalls at 20
-- maybe priority queue where priority = robot count. ore worth 1, clay worth 2, 

-- need a way to cut the search space, it grows exponentially... :/

part1 = do
    lines_ <- getLines "day19/input.txt"
    let raw = map (splitOneOf ":.") lines_
    let (ids, prints_) = unzip $ map (fromJust . uncons) raw
    let prints = map init prints_
    let blueprints = map (flip parseBlueprint $ Map.empty) prints
    let rootState = State { robots = [Robot Ore], resources = emptyRC, minutes = 0 }
    let neighborsOfFirst = neighbors rootState (head blueprints)
    print $ filter (`notElem` (Set.singleton rootState)) $ neighbors rootState (head blueprints)
    print $ bfs (head blueprints) (PSQ.singleton rootState 0) (Set.singleton rootState) 0
    -- let bp = head blueprints
    -- print $ (length $ filter (>= 0) (Map.elems $ subtractResources (Map.fromList [(Ore, 4)]) (bp Map.! Robot Ore))) > 0
    -- print $ addResources Map.empty $ mineralsToRC $ map mineral [Robot Ore]
    -- print $ availableRobots (Map.fromList [(Ore, 1)]) (head blueprints) [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]