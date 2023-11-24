{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
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
        newRobotPossibilities = filter (\robot -> (resourcesProducedByRobot robot (mins+1) Map.! mineral robot) > Map.findWithDefault 0 (mineral robot) (bp Map.! robot)) $
            availableRobots rc bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]
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
    sum (zipWith (^) (map (oreEquivalent bp) (Map.keys $ bp Map.! Robot m)) (Map.elems $ bp Map.! Robot m))

oreEquivalentMap :: Blueprint -> ResourceCount -> Int
oreEquivalentMap bp m = sum (zipWith (^) (map (oreEquivalent bp) (Map.keys m)) (Map.elems m))

-- true if a's resources are all >= b's resources for each type
-- NOPE! This check is too simple. 1 ore, 7 clay, 1 obsidian should be better than 1 ore, 10 clay, 0 obsidian
-- has doesn't is automatic yes
-- can we ignore clay since it doesn't directly factor into geode robots? i don't think so
resourcesBetter :: ResourceCount -> ResourceCount -> Blueprint -> Bool
resourcesBetter a b bp =
    (highestResource b <= highestResource a) &&
    (not (all (<= 0) (Map.elems $ subtractResources a b)) ||
    all (== 0) (Map.elems $ subtractResources a b))
    && ((b Map.! Geode) <= (a Map.! Geode))

robotsBetter :: MultiSet.MultiSet Robot -> MultiSet.MultiSet Robot -> Bool
robotsBetter a b =
    let diff = Map.unionWith (-) (Map.fromList (MultiSet.toOccurList a)) (Map.fromList (MultiSet.toOccurList b)) in
        (not (all (<= 0) $ Map.elems $ diff)) || (all (== 0) $ Map.elems $ diff)

-- ok so
-- when would we not want to buy a new-resource-type robot
-- since this fails on example 2
-- potentially buying two obsidian robots before a geode robot is good
-- Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
-- since 12 obsidian is a lot
-- if this can be turned into a shortest path finder through the state graph that's REALLY good
-- wait maybe you can? movement cost equals cost of buying robot
-- but then it would do nothing since that's the lowest cost move
-- is this like the lanternfish?
-- also maybe there is a way to evaluate the state to minute 24 right when you create a robot
-- like adding all the resources you would collect since that's invariant
-- so what if it is shortest-path where movement cost is like benefit to cost ratio
-- total resources generated

-- what can we safely assume
-- 1. if two states have the same robots, choose the one with more resources
-- 2. if two states have the same resources, choose the one with more robots

-- something is wrong with pruning since
-- State {robots = fromOccurList [(Robot Ore,5),(Robot Clay,3),(Robot Obsidian,1)], resources = fromList [(Ore,17),(Clay,17),(Obsidian,0),(Geode,0)], minutes = 15}
-- then State {robots = fromOccurList [(Robot Ore,5),(Robot Clay,3),(Robot Obsidian,1)], resources = fromList [(Ore,17),(Clay,17),(Obsidian,2),(Geode,0)], minutes = 15}
-- should only pick the second one
{-
this is the problem
State {robots = fromOccurList [(Robot Ore,5),(Robot Clay,3),(Robot Obsidian,1)], resources = fromList [(Ore,27),(Clay,13),(Obsidian,1),(Geode,0)], minutes = 15} fromList [(Ore,24),(Clay,12),(Obsidian,3),(Geode,0)]
State {robots = fromOccurList [(Robot Ore,5),(Robot Clay,3),(Robot Obsidian,1)], resources = fromList [(Ore,25),(Clay,13),(Obsidian,2),(Geode,0)], minutes = 15} fromList [(Ore,24),(Clay,12),(Obsidian,3),(Geode,0)]
State {robots = fromOccurList [(Robot Ore,5),(Robot Clay,3),(Robot Obsidian,1)], resources = fromList [(Ore,27),(Clay,13),(Obsidian,0),(Geode,0)], minutes = 15} fromList [(Ore,24),(Clay,12),(Obsidian,3),(Geode,0)]

the third one should be eliminated based on the first one
do we really have to check all other same-robot states?
-}

-- one of these should not be considered
-- State {robots = fromOccurList [(Robot Ore,6),(Robot Clay,8)], resources = fromList [(Ore,18),(Clay,42),(Obsidian,0),(Geode,0)], minutes = 18}
-- State {robots = fromOccurList [(Robot Ore,6),(Robot Clay,7)], resources = fromList [(Ore,21),(Clay,42),(Obsidian,0),(Geode,0)], minutes = 18}

-- this is dumb... built another robot and have less of the resource
{-
State {robots = fromOccurList [(Robot Ore,7),(Robot Clay,8)], resources = fromList [(Ore,24),(Clay,64),(Obsidian,0),(Geode,0)], minutes = 21}
State {robots = fromOccurList [(Robot Ore,8),(Robot Clay,8)], resources = fromList [(Ore,20),(Clay,64),(Obsidian,0),(Geode,0)], minutes = 21}
-}

-- so if cost > total benefit it's not worth it, but that only applies to late minutes

-- State {robots = fromOccurList [(Robot Ore,6),(Robot Clay,5),(Robot Obsidian,4),(Robot Geode,1)], resources = fromList [(Ore,25),(Clay,30),(Obsidian,17),(Geode,3)], minutes = 21}
-- State {robots = fromOccurList [(Robot Ore,6),(Robot Clay,5),(Robot Obsidian,4),(Robot Geode,2)], resources = fromList [(Ore,22),(Clay,30),(Obsidian,5),(Geode,3)], minutes = 21}
-- here we want to pick the second state because same amount of resources corresponding to one robot type but more robots (geodes in this case)
-- so if during one specific minute robot count is greater and resource count is equal then that is very good

highestRobot :: MultiSet.MultiSet Robot -> Robot
highestRobot set
  | MultiSet.member (Robot Geode) set = Robot Geode
  | MultiSet.member (Robot Obsidian) set = Robot Obsidian
  | MultiSet.member (Robot Clay) set = Robot Clay
  | otherwise = Robot Ore

highestResource :: ResourceCount -> Mineral
highestResource map
  | (Map.!) map Geode > 0 = Geode
  | (Map.!) map Obsidian > 0 = Obsidian
  | (Map.!) map Clay > 0 = Clay
  | otherwise = Ore

resourcesProducedByRobot :: Robot -> Int -> ResourceCount
resourcesProducedByRobot (Robot mineral) minutes = Map.insert mineral (24 - minutes) emptyRC

-- bfs blueprint frontier visited maximumGeodes -> maximumGeodes for "tree branches" below
-- the map: key = (robots, minutes), value = (resources)
bfs :: Blueprint -> [State] -> Set.Set State -> Int -> Int -> Int -> Map.Map (MultiSet.MultiSet Robot, Int) [ResourceCount] -> Map.Map (ResourceCount, Int) (MultiSet.MultiSet Robot) -> Map.Map Int Robot -> Int
bfs bp frontier_ visited maximumGeodes maxGeodeMins lastMins robotResourceMap resourceRobotMap highestRobotMap =
    if null frontier_ then trace "frontier empty" maximumGeodes
    else
        let (state:frontier) = frontier_ in
            if not (all (\s -> resourcesBetter (resources state) s bp) (Map.findWithDefault [] (robots state, minutes state) robotResourceMap)) then {-(trace $ "rejected " ++ show (Map.findWithDefault minusOneRC (robots state, minutes state) robotResourceMap) ++ " " ++ show state)-}
                bfs bp frontier (Set.insert state visited) maximumGeodes maxGeodeMins lastMins robotResourceMap resourceRobotMap highestRobotMap
            else if not (robotsBetter (robots state) (Map.findWithDefault MultiSet.empty (resources state, minutes state) resourceRobotMap)) then
                bfs bp frontier (Set.insert state visited) maximumGeodes maxGeodeMins lastMins robotResourceMap resourceRobotMap highestRobotMap
            else
                -- well I was using `notElem` instead of `Set.notMember`... caused a **huge** slowdown converting to list
                let origNeighs = filter (`Set.notMember` visited) $
                        neighbors state bp maximumGeodes maxGeodeMins
                    resourceNeighs = filter (\st -> all (\s -> resourcesBetter (resources st) s bp) (Map.findWithDefault [] (robots st, minutes st) robotResourceMap)) origNeighs
                    robotNeighs = filter (\s -> robotsBetter (robots s) (Map.findWithDefault MultiSet.empty (resources s, minutes s) resourceRobotMap)) origNeighs
                    neighs = filter (\s -> robotsBetter (robots s) (Map.findWithDefault MultiSet.empty (resources s, minutes s) resourceRobotMap)) resourceNeighs
                        -- filter (\s -> MultiSet.member (Map.findWithDefault (Robot Ore) (minutes s) highestRobotMap) (robots s)) $
                    newMax = max maximumGeodes $ (Map.!) (resources state) Geode
                    newSet = foldl (flip Set.insert) visited neighs
                    newRobotResourceMap = Map.unionWith (++)
                                                    (Map.fromList (map (\s -> ((robots s, minutes s), [resources s])) resourceNeighs))
                                                    (if lastMins == minutes state then robotResourceMap else trace ("Now on minute " ++ show (minutes state) ++ " " ++ show state ++ " " ++ show (Map.size robotResourceMap)) robotResourceMap)
                    newResourceRobotMap = Map.union
                                                    (Map.fromList (map (\s -> ((resources s, minutes s), robots s)) robotNeighs))
                                                    resourceRobotMap
                    newHighestRobotMap = {-Map.insert (minutes state + 1) (max (maximum (map (\m -> Map.findWithDefault (Robot Ore) m highestRobotMap) [0..(minutes state + 1)])) (maximum (Robot Ore:map (highestRobot . robots) neighs)))-} highestRobotMap
                    in (trace $ show state{- ++ " " ++ show (Map.findWithDefault [] (robots state, minutes state) robotResourceMap)-}) (
                    if newMax > maximumGeodes || (newMax == maximumGeodes && minutes state == 24) then {-(trace $ show state)-}
                        (max newMax $ bfs bp (frontier ++ neighs) newSet newMax (minutes state) (minutes state) newRobotResourceMap newResourceRobotMap newHighestRobotMap)

                    else max newMax $ bfs bp (frontier ++ neighs) newSet newMax maxGeodeMins (minutes state) newRobotResourceMap newResourceRobotMap newHighestRobotMap)

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
    let maxGeodes = map (\bp -> bfs bp [rootState] (Set.singleton rootState) 0 0 0 Map.empty Map.empty Map.empty) blueprints
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
-- 1184 high
-- 1031 low
-- 1076?