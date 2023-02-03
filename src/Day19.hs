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
newtype Robot = Robot Mineral deriving (Ord, Show, Eq)
type Cost = Map.Map Mineral Int
type ResourceCount = Map.Map Mineral Int
type Blueprint = Map.Map Robot Cost
data State = State { robots :: [Robot], resources :: ResourceCount, minutes :: Int } deriving (Show, Ord, Eq)
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
availableRobots rc bp rs = filter (\robot -> (length $ filter (< 0) (Map.elems $ subtractResources rc (bp Map.! robot))) == 0) rs

mineralsToRC :: [Mineral] -> ResourceCount
mineralsToRC ms = Map.fromListWith (+) (map (\x -> (x, 1)) ms)

neighbors :: State -> Blueprint -> Int -> Int -> [State]
neighbors State { minutes = 25 } _ _ _ = [] -- likely needs to be changed for part 2
neighbors State { robots = rs, resources = rc, minutes = mins } bp maxGeodes maxGeodeMins = 
    let newResources = addResources rc $ mineralsToRC $ map mineral rs
        newRobotPossibilities = availableRobots rc bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode]
        prio = priority bp (Robot Geode) State { robots = rs, resources = rc, minutes = mins }
        newState = State { robots = rs, resources = newResources, minutes = mins + 1 } in
            -- 1. find robots which can be created (mineral count > mineral need)
            -- 2. get states with those robots
            -- there is also the state where just collection happens
            -- if (Robot Geode `elem` newRobotPossibilities){- && (Robot Geode `notElem` rs) -}then filter (shouldntBeCut bp maxGeodes) $ {-State { robots = rs, resources = newResources, minutes = mins + 1 }:-}[State { robots = Robot Geode:rs, resources = subtractResources newResources (bp Map.! Robot Geode), minutes = mins + 1 }]
            -- else if (Robot Obsidian `elem` newRobotPossibilities) && (Robot Obsidian `notElem` rs) then filter (shouldntBeCut bp maxGeodes) $ {-State { robots = rs, resources = newResources, minutes = mins + 1 }:-}[State { robots = Robot Obsidian:rs, resources = subtractResources newResources (bp Map.! Robot Obsidian), minutes = mins + 1 }]
            -- else if (Robot Obsidian `elem` newRobotPossibilities) then filter (shouldntBeCut bp maxGeodes) $ State { robots = rs, resources = newResources, minutes = mins + 1 }:[State { robots = Robot Obsidian:rs, resources = subtractResources newResources (bp Map.! Robot Obsidian), minutes = mins + 1 }]
            -- else if (Robot Clay `elem` newRobotPossibilities) && (Robot Clay `notElem` rs) then filter (shouldntBeCut bp maxGeodes) $ {-State { robots = rs, resources = newResources, minutes = mins + 1 }:-}[State { robots = Robot Clay:rs, resources = subtractResources newResources (bp Map.! Robot Clay), minutes = mins + 1 }]
            -- else if (Robot Clay `elem` newRobotPossibilities) then filter (shouldntBeCut bp maxGeodes) $ State { robots = rs, resources = newResources, minutes = mins + 1 }:[State { robots = Robot Clay:rs, resources = subtractResources newResources (bp Map.! Robot Clay), minutes = mins + 1 }]
            -- filter (shouldntBeCut bp maxGeodes) $ State { robots = rs, resources = newResources, minutes = mins + 1 } : map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities -- ugh, need to subtract resources from newResources for every robot...
            {-if (True || shouldntBeCut bp maxGeodes newState) && Robot Geode `elem` newRobotPossibilities then newState:[State { robots = Robot Geode:rs, resources = subtractResources newResources (bp Map.! Robot Geode), minutes = mins + 1 }]
            else -}
            
            filter (shouldntBeCut bp maxGeodes maxGeodeMins) $ newState:map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities

            -- if Robot Geode `elem` newRobotPossibilities then {-newState:-}[State { robots = Robot Geode:rs, resources = subtractResources newResources (bp Map.! Robot Geode), minutes = mins + 1 }]
            -- else if isNothing prio then filter (shouldntBeCut bp maxGeodes) $ newState : map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities
            -- --else if isNothing prio then map (\robot -> State { robots = robot:rs, resources = subtractResources newResources (bp Map.! robot), minutes = mins + 1 }) newRobotPossibilities
            -- else filter (shouldntBeCut bp maxGeodes) $ newState:[State { robots = fromJust prio:rs, resources = subtractResources newResources (bp Map.! fromJust prio), minutes = mins + 1 }]
            -- --else [State { robots = fromJust prio:rs, resources = subtractResources newResources (bp Map.! fromJust prio), minutes = mins + 1 }]

shouldntBeCut :: Blueprint -> Int -> Int -> State -> Bool
shouldntBeCut b i i2 s = not $ shouldBeCut b i i2 s

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

-- what if prioritized by ore equivalent?
-- try cutting all states with fewer geodes than current maximum at minutes for max geodes

shouldBeCut :: Blueprint -> Int -> Int -> State -> Bool
shouldBeCut bp maxGeodes maxGeodeMins State { robots = rs, minutes = mins, resources = rc } = 
    let totalOre = ((length $ filter (== Robot Ore) rs) * (24-mins)) + (rc Map.! Ore)
        totalClay = ((length $ filter (== Robot Clay) rs) * (24-mins)) + (rc Map.! Clay)
        totalObsidian = ((length $ filter (== Robot Obsidian) rs) * (24-mins)) + (rc Map.! Obsidian)
        totalGeode = ((length $ filter (== Robot Geode) rs) * (24-mins)) + (rc Map.! Geode)
        biggestOreCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Ore) (Map.elems bp))
        biggestClayCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Clay) (Map.elems bp))
        biggestObsidianCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Obsidian) (Map.elems bp))
        oreEquiv = oreEquivalent bp Ore * totalOre + oreEquivalent bp Clay * totalClay + oreEquivalent bp Obsidian * totalObsidian-- + oreEquivalent bp Geode * totalGeode
        a = totalOre > (biggestOreCost * (24 - mins))
        b = totalClay > (biggestClayCost * (24 - mins))
        c = totalObsidian > (biggestObsidianCost * (24 - mins)) in
            -- (totalGeode * 2) < maxGeodes ||
            {-((oreEquiv > (oreEquivalentMap bp (bp Map.! Robot Geode) * (24 - mins)))
            || -}((a || b || c))-- && (trace $ "cutting" ++ show ((length $ filter (== (Robot Ore)) (rs))) ++ " " ++ show ((length $ filter (== (Robot Clay)) (rs))) ++ " " ++ show ((length $ filter (== (Robot Obsidian)) (rs))) ++ " " ++ show ((length $ filter (== (Robot Geode)) (rs)))) True
            && ({-totalGeode == 0 || -}(rc Map.! Geode < maxGeodes && mins >= maxGeodeMins))
            -- || oreEquiv > (oreEquivalentMap bp (bp Map.! Robot Geode) * (24 - mins))

{-# SCC shouldBeCut #-}
{-# SCC priority #-}

oreEquivalent :: Blueprint -> Mineral -> Int
oreEquivalent bp Ore = 1
oreEquivalent bp m =
    sum (zipWith (*) (map (oreEquivalent bp) (Map.keys $ bp Map.! Robot m)) (Map.elems $ bp Map.! Robot m))

oreEquivalentMap :: Blueprint -> ResourceCount -> Int
oreEquivalentMap bp m = sum (zipWith (*) (map (oreEquivalent bp) (Map.keys m)) (Map.elems m))

-- -- maybe prioritize based on have vs need for geode bot?
-- -- if past state (in minutes) that got first geode bot and have no geode bots then delete
-- -- consider making the blueprint robot list a set
-- priority :: Blueprint -> PSQ.PSQ State Int -> State -> Int
-- priority bp q State { robots = rs, minutes = mins, resources = rc } = 
--     let totalOre = (length $ filter (== (Robot Ore)) rs) * (24-mins) + (rc Map.! Ore)
--         totalClay = (length $ filter (== (Robot Clay)) rs) * (24-mins) + (rc Map.! Clay)
--         totalObsidian = (length $ filter (== (Robot Obsidian)) rs) * (24-mins) + (rc Map.! Obsidian)
--         totalGeode = (length $ filter (== (Robot Geode)) rs) * (24-mins) + (rc Map.! Geode)
--         biggestOreCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Ore) (Map.elems bp))
--         biggestClayCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Clay) (Map.elems bp))
--         biggestObsidianCost = maximum (map (\m -> fromMaybe 0 $ m Map.!? Obsidian) (Map.elems bp))
--         oreEquiv = (oreEquivalent bp Ore) * totalOre + (oreEquivalent bp Clay) * totalClay + (oreEquivalent bp Obsidian) * totalObsidian * (oreEquivalent bp Geode) * totalGeode
--         baseline = 1000000000
--         frontierStates = PSQ.keys q
--         frontierRobots = map (sort . robots) frontierStates in
--             -- don't stay at low/ore. if you look at the sample input that doesn't happen
--             -- baseline - (totalGeode * 10) + (rc Map.! Ore) + mins
--             mins{- - totalGeode * 1000 - if (sort rs) `notElem` frontierRobots then 100 else 0-}
--             {- - (totalClay * 2) - totalOre-} -- BFS = +mins (lower priority), DFS = -mins (higher priority)
--     -- 1000000 - -- ((rc Map.! Geode) * 10) + (rc Map.! Ore) + mins
--     -- (
--     --     (length $ filter (== (Robot Ore)) rs) * 2 + -- 4
--     --     ((length $ (filter (== (Robot Clay)) rs)) * 0) + -- 2
--     --     ((length $ filter (== (Robot Obsidian)) rs) * 7) + -- 17
--     --     ((length $ filter (== (Robot Geode)) rs) * 100)) + (mins*2) + ((rc Map.! Geode)*100)

-- Not sure what the exit condition should be. Maybe # of iterations with that maximum?

-- blueprint 7 stalls

-- goal: get geodes
-- by: making geode bots
-- goal: make geode bots
-- by: collecting ore and obsidian
-- goal: collect ore     goal: collect obsidian
-- by: using 1+ ore bots by: using 1+ obsidian bots
-- goal: make ore bots   goal: make obsidian bots
-- by: using initial     by: collecting ore and clay
--                       goal: collect clay
--                       by: making clay bots from ore

-- so maybe solve it by what gets closer to what we need

-- input: blueprint, robot you want, current state
-- output: best robot to construct to get there
priority :: Blueprint -> Robot -> State -> Maybe Robot
-- priority _ (Robot Ore) _ = Nothing
-- priority _ (Robot Clay) _ = Nothing
priority bp r State { robots = rs, resources = rc, minutes = mins } = {-(trace $ "prio" ++ show r ++ show rc)-}
    let nextResources = addResources rc $ mineralsToRC $ map mineral rs
        resourcesNeeded = Map.filter (>0) $ subtractResources (bp Map.! r) nextResources in
            if null resourcesNeeded then Just r
            else
                let maxPriority = Robot (fst $ maximumBy (\a b -> snd a `compare` snd b) $ map (\e -> (e, oreEquivalent bp e)) (Map.keys resourcesNeeded))
                    newRobotPossibilities = availableRobots rc bp [Robot Ore, Robot Clay, Robot Obsidian, Robot Geode] in
                        if maxPriority `elem` newRobotPossibilities then Just maxPriority
                        else if r `elem` rs then Nothing
                        else priority bp maxPriority State { robots = rs, resources = rc, minutes = mins }

-- bfs blueprint frontier visited maximumGeodes -> maximumGeodes for "tree branches" below
bfs :: Blueprint -> [State] -> Set.Set State -> Int -> Int -> Int
bfs bp frontier_ visited maximumGeodes maxGeodeMins =
    if null frontier_ then trace "frontier empty" maximumGeodes
    else
        let (state:frontier) = frontier_
        -- well I was using `notElem` instead of `Set.notMember`... caused a **huge** slowdown converting to list
            neighs = filter (`Set.notMember` visited) $ neighbors state bp maximumGeodes maxGeodeMins
            newMax = maximum (maximumGeodes:map (\s -> (Map.!) (resources s) Geode) neighs)
            newSet = {-Set.union visited (Set.fromList neighs)-}foldl (flip Set.insert) visited neighs in {-(trace $ show (minutes state) ++ " " ++ show ((resources state) Map.! Geode) ++ " " ++ show maximumGeodes ++ " " ++ show ((length $ filter (== (Robot Ore)) (robots state))) ++ " " ++ show ((length $ filter (== (Robot Clay)) (robots state))) ++ " " ++ show ((length $ filter (== (Robot Obsidian)) (robots state))) ++ " " ++ show ((length $ filter (== (Robot Geode)) (robots state))) ++ " " ++ show (length frontier))-} (
                if newMax > maximumGeodes then (trace $ show newMax ++ " " ++ show (length frontier) ++ show (map minutes neighs) ++ show (map ((length . filter (== Robot Geode)) . robots) neighs)) (max newMax $ bfs bp (frontier ++ neighs) newSet newMax (maximum (map minutes neighs)))
                else max newMax $ bfs bp (frontier ++ neighs) {-Set.empty-}{-(foldl (flip Set.insert) visited neighs)-}newSet newMax maxGeodeMins)

-- now it stalls at 20
-- maybe priority queue where priority = robot count. ore worth 1, clay worth 2, 

-- need a way to cut the search space, it grows exponentially... :/

part1 = do
    lines_ <- getLines "day19/input.txt"
    let raw = map (splitOneOf ":.") lines_
    let (ids, prints_) = unzip $ map (fromJust . uncons) raw
    let prints = map init prints_
    let blueprints = map (flip parseBlueprint $ Map.empty) prints
    let rootState = State { robots = [Robot Ore], resources = emptyRC, minutes = 0 } -- should probably be zero
    let neighborsOfFirst = neighbors rootState (head blueprints)
    let maxGeodes = map (\bp -> bfs bp [rootState] (Set.singleton rootState) 1 0) blueprints
    let qualityLevels = zipWith (*) [1..(length maxGeodes)] maxGeodes
    print $ oreEquivalent (head blueprints) Ore
    print $ oreEquivalent (head blueprints) Clay
    print $ oreEquivalent (head blueprints) Obsidian
    print $ oreEquivalent (head blueprints) Geode
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