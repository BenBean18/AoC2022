module Day19 where

import qualified Data.Map as Map
import Text.Regex.Base
import Text.Regex.PCRE
import Utilities
import Data.List.Split
import Data.List (uncons, unzip, sort, maximumBy)
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import qualified Data.PSQueue as PSQ
import qualified Data.MultiSet as MultiSet

-- Use something similar to alpha-beta pruning
-- If a move is worse than a previously examined move, don't follow that path

-- wait...
-- if you have the same robots but less resources then that's a bad path

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
newtype Robot = Robot Mineral deriving (Ord, Show, Eq)
type Cost = Map.Map Mineral Int
type ResourceCount = Map.Map Mineral Int
type Blueprint = Map.Map Robot Cost
data State = State { robots :: MultiSet.MultiSet Robot, resources :: ResourceCount, minutes :: Int } deriving (Show, Ord, Eq)
-- instance Eq State where
--     --State {robots = r1, resources = rc1, minutes = m1} == State {robots = r2, resources = rc2, minutes = m2} = not (m1 /= m2 || rc1 /= rc2 || r1 /= r2)
--     State {robots = r1, resources = rc1, minutes = m1} == State {robots = r2, resources = rc2, minutes = m2} = rc1 == rc2 && m1 == m2 && r1 == r2 -- try saying states are duplicates if they have the same robot count. it's an oversimplification but it could work?

mineral :: Robot -> Mineral
mineral (Robot m) = m

subtractResources :: ResourceCount -> ResourceCount -> ResourceCount
-- subtractResources r1 r2 = map (\k -> (r1 Map.! k) - (r2 Map.! k)) (Map.keys r1)
subtractResources = Map.unionWith (-)

addResources :: ResourceCount -> ResourceCount -> ResourceCount
-- addResources r1 r2 = map (\k -> (r1 Map.! k) + (r2 Map.! k)) (Map.keys r1)
addResources = Map.unionWith (+)

-- newState :: State -> Robot -> Blueprint -> State
-- newState State { robots = rs, resources = rc } r bp = State { robots = r:rs, resources = rc `subtractResources` (bp Map.! r) }

availableRobots :: ResourceCount -> Blueprint -> [Robot] -> [Robot]
availableRobots rc bp = filter (\robot -> not (any (< 0) (Map.elems $ subtractResources rc (bp Map.! robot))))

mineralsToRC :: [Mineral] -> ResourceCount
mineralsToRC ms = Map.fromListWith (+) (map (\x -> (x, 1)) ms)

neighbors :: State -> Blueprint -> Int -> Int -> [State]
neighbors State { minutes = 24 } _ _ _ = [] -- likely needs to be changed for part 2
neighbors State { robots = rs, resources = rc, minutes = mins } bp maxGeodes maxGeodeMins =
    let newResources = addResources rc $ mineralsToRC $ map mineral (MultiSet.toList rs)
        newRobotPossibilities = availableRobots rc bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]
        newState = State { robots = rs, resources = newResources, minutes = mins + 1 } in

            newState:map (\robot -> State { robots = MultiSet.insert robot rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities


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

minusOneRC :: ResourceCount
minusOneRC = Map.fromList [(Ore, -1), (Clay, -1), (Obsidian, -1), (Geode, -1)]

-- If two states have the same robots and one has "better" resources, use that one
-- How do we define "better" resources?

oreEquivalent :: Blueprint -> Mineral -> Int
oreEquivalent bp Ore = 1
oreEquivalent bp m =
    sum (zipWith (*) (map (oreEquivalent bp) (Map.keys $ bp Map.! Robot m)) (Map.elems $ bp Map.! Robot m))

oreEquivalentMap :: Blueprint -> ResourceCount -> Int
oreEquivalentMap bp m = sum (zipWith (*) (map (oreEquivalent bp) (Map.keys m)) (Map.elems m))

-- true if a's resources are all >= b's resources for each type
-- NOPE! This check is too simple. 1 ore, 7 clay, 1 obsidian should be better than 1 ore, 10 clay, 0 obsidian
-- has doesn't is automatic yes
resourcesBetter :: ResourceCount -> ResourceCount -> Blueprint -> Bool
resourcesBetter a b bp =
    (not (all (<= 0) $ Map.elems $ subtractResources a b)) || (all (== 0) $ Map.elems $ subtractResources a b)

-- bfs blueprint frontier visited maximumGeodes -> maximumGeodes for "tree branches" below
-- the map: key = (robots, minutes), value = (resources)
bfs :: Blueprint -> [State] -> Set.Set State -> Int -> Int -> Int -> Map.Map (MultiSet.MultiSet Robot, Int) ResourceCount -> Int
bfs bp frontier_ visited maximumGeodes maxGeodeMins lastMins robotResourceMap =
    if null frontier_ then trace "frontier empty" maximumGeodes
    else
        let (state:frontier) = frontier_ in
            if not (resourcesBetter (resources state) (Map.findWithDefault minusOneRC (robots state, minutes state) robotResourceMap) bp) then (trace $ "rejected " ++ show (Map.findWithDefault minusOneRC (robots state, minutes state) robotResourceMap) ++ " " ++ show state)
                bfs bp frontier (Set.insert state visited) maximumGeodes maxGeodeMins lastMins robotResourceMap
            else
                -- well I was using `notElem` instead of `Set.notMember`... caused a **huge** slowdown converting to list
                let neighs = filter (\s -> resourcesBetter (resources s) (Map.findWithDefault minusOneRC (robots s, minutes s) robotResourceMap) bp ) $
                        filter (`Set.notMember` visited) $
                        neighbors state bp maximumGeodes maxGeodeMins
                    newMax = max maximumGeodes $ (Map.!) (resources state) Geode
                    newSet = foldl (flip Set.insert) visited neighs
                    newRobotResourceMap = Map.union
                                                    (Map.fromList (map (\s -> ((robots s, minutes s), resources s)) neighs))
                                                    (if lastMins == minutes state then robotResourceMap else trace ("Now on minute " ++ show (minutes state) ++ " " ++ show state) robotResourceMap) in {-(trace $ show state ++ " " ++ show neighs)-} (
                    if newMax > maximumGeodes || (newMax == maximumGeodes && minutes state == 24) then {-(trace $ show state)-}
                        (max newMax $ bfs bp (frontier ++ neighs) newSet newMax (minutes state) (minutes state) newRobotResourceMap)

                    else max newMax $ bfs bp (frontier ++ neighs) newSet newMax maxGeodeMins (minutes state) newRobotResourceMap)

-- \(Robot Ore,1\),\(Robot Clay,4\),\(Robot Obsidian,2\),\(Robot Geode,2\)[^\[]+\[\(Ore,6\),\(Clay,41\)

-- now it stalls at 20
-- maybe priority queue where priority = robot count. ore worth 1, clay worth 2, 

-- need a way to cut the search space, it grows exponentially... :/

part1 = do
    lines_ <- getLines "day19/input.txt"
    let raw = map (splitOneOf ":.") lines_
    let (ids, prints_) = unzip $ map (fromJust . uncons) raw
    let prints = map init prints_
    let blueprints = map (`parseBlueprint` Map.empty) prints
    let rootState = State { robots = MultiSet.fromList [Robot Ore], resources = emptyRC, minutes = 0 } -- should probably be zero
    let neighborsOfFirst = neighbors rootState (head blueprints)
    let maxGeodes = map (\bp -> bfs bp [rootState] (Set.singleton rootState) 1 0 0 Map.empty) blueprints
    let qualityLevels = zipWith (*) [1..(length maxGeodes)] maxGeodes
    print (head blueprints)
    print maxGeodes
    print [1..(length maxGeodes)]
    print $ sum qualityLevels
    -- oddly this is double...
    --print $ bfs (blueprints !! 3) (PSQ.singleton rootState 0) (Set.singleton rootState) 0
    -- let bp = head blueprints
    -- print $ (length $ filter (>= 0) (Map.elems $ subtractResources (Map.fromList [(Ore, 4)]) (bp Map.! Robot Ore))) > 0
    -- print $ addResources Map.empty $ mineralsToRC $ map mineral [Robot Ore]
    -- print $ availableRobots (Map.fromList [(Ore, 1)]) (head blueprints) [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]

-- 3751 high
-- 3600 high
-- 2802 high