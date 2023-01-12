module Day19 where

import qualified Data.Map as Map
import Text.Regex.Base
import Text.Regex.PCRE
import Utilities
import Data.List.Split
import Data.List (uncons, unzip)
import Data.Maybe

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
data State = State { robots :: [Robot], resources :: ResourceCount } deriving (Eq, Show, Ord)

mineral :: Robot -> Mineral
mineral (Robot m) = m

subtractResources :: ResourceCount -> ResourceCount -> ResourceCount
-- subtractResources r1 r2 = map (\k -> (r1 Map.! k) - (r2 Map.! k)) (Map.keys r1)
subtractResources r1 r2 = Map.unionWith (-) r1 r2

addResources :: ResourceCount -> ResourceCount -> ResourceCount
-- addResources r1 r2 = map (\k -> (r1 Map.! k) + (r2 Map.! k)) (Map.keys r1)
addResources r1 r2 = Map.unionWith (+) r1 r2

newState :: State -> Robot -> Blueprint -> State
newState State { robots = rs, resources = rc } r bp = State { robots = r:rs, resources = rc `subtractResources` (bp Map.! r) }

availableRobots :: ResourceCount -> Blueprint -> [Robot] -> [Robot]
availableRobots rc bp rs = filter (\robot -> (length $ filter (< 0) (Map.elems $ subtractResources rc (bp Map.! robot))) == 0) rs

mineralsToRC :: [Mineral] -> ResourceCount
mineralsToRC ms = Map.fromListWith (+) (map (\x -> (x, 1)) ms)

neighbors :: State -> Blueprint -> [State]
neighbors State { robots = rs, resources = rc } bp = 
    let newResources = addResources rc $ mineralsToRC $ map mineral rs
        newRobotPossibilities = availableRobots newResources bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode] in
            -- 1. find robots which can be created (mineral count > mineral need)
            -- 2. get states with those robots
            -- there is also the state where just collection happens
            State { robots = rs, resources = newResources } : map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot) }) newRobotPossibilities -- ugh, need to subtract resources from newResources for every robot...

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

part1 = do
    lines_ <- getLines "day19/input.txt"
    let raw = map (splitOneOf ":.") lines_
    let (ids, prints_) = unzip $ map (fromJust . uncons) raw
    let prints = map init prints_
    let blueprints = map (flip parseBlueprint $ Map.empty) prints
    let neighborsOfFirst = neighbors (State { robots = [Robot Ore], resources = Map.insert Ore 2 emptyRC }) (head blueprints)
    print $ neighborsOfFirst
    -- let bp = head blueprints
    -- print $ (length $ filter (>= 0) (Map.elems $ subtractResources (Map.fromList [(Ore, 4)]) (bp Map.! Robot Ore))) > 0
    -- print $ addResources Map.empty $ mineralsToRC $ map mineral [Robot Ore]
    -- print $ availableRobots (Map.fromList [(Ore, 1)]) (head blueprints) [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]