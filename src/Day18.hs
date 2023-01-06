module Day18 where

import Utilities
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Split
import Data.List (sort, group, (\\), maximumBy)
import Debug.Trace
import Criterion.Main
import System.Environment
-- 0 0 0 Up
-- 0 0 1 Down
data Point = Point Int Int Int deriving (Show, Ord, Eq)
data Face = Up | R | Forward | Down | L | Back deriving (Enum, Show, Eq, Bounded, Ord)
data Side = Side Point Face deriving (Show)
instance Eq Side where
    (Side p1 f1) == (Side p2 f2) = (p1 == p2 && f1 == f2) || (go p1 f1 == p2 && f1 == oppositeFace f2)

pt (Side p _) = p
face (Side _ f) = f
instance Ord Side where
    (Side p1 f1) `compare` (Side p2 f2) = 
        let side1Pt = go p1 f1
            side2Pt = go p2 f2
            side1Max = if side1Pt > p1 then (Side side1Pt (oppositeFace f1)) else (Side p1 f1)
            side2Max = if side2Pt > p2 then (Side side2Pt (oppositeFace f2)) else (Side p2 f2)
            in
                if side1Max == side2Max then EQ else
                    if pt side1Max `compare` pt side2Max == EQ then face side1Max `compare` face side2Max else pt side1Max `compare` pt side2Max

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

-- need difference of sides and Set.toList $ Set.fromList sides
-- e.g. sides = [1,2,2,3]
--   sidesSet = [1,2,3]

-- difference: [2]
-- difference between sidesSet and difference: [1,3]

-- need to be able to sort so duplicates are next to each other
addCubes :: [Point] -> [Side]
addCubes pts = 
    let sides = [Side p face | face <- [minBound..maxBound], p <- pts]
        sidesSet = Set.toList $ Set.fromList sides
        difference = sides \\ sidesSet in sidesSet \\ difference

parseCube :: String -> Point
parseCube s = let [x,y,z] = map (\x -> read x :: Int) $ splitOn "," s in (Point x y z)

part1 = do
    lines <- getLines "day18/input.txt"
    let cubes = map parseCube lines
    let unique = addCubes cubes
    print $ length unique

-- Part 2

-- Need to find 8 cubes arranged in a box with none inside...
-- A side is on the outside if the cube that it `go`es to has no sides on it

doesPointExceedUpper :: Point -> Point -> Bool
doesPointExceedUpper (Point x1 y1 z1) (Point x2 y2 z2) = x1 > x2 || y1 > y2 || z1 > z2

doesPointExceedLower = flip doesPointExceedUpper

doesPointExceed p1 p2 = doesPointExceedUpper p1 p2 || doesPointExceedLower p1 p2

maxX :: [Point] -> Point
maxX pts = maximumBy (\(Point x1 _ _) (Point x2 _ _) -> x1 `compare` x2) pts

maxY :: [Point] -> Point
maxY pts = maximumBy (\(Point _ y1 _) (Point _ y2 _) -> y1 `compare` y2) pts

maxZ :: [Point] -> Point
maxZ pts = maximumBy (\(Point _ _ z1) (Point _ _ z2) -> z1 `compare` z2) pts

minX :: [Point] -> Point
minX pts = maximumBy (\(Point x1 _ _) (Point x2 _ _) -> x2 `compare` x1) pts

minY :: [Point] -> Point
minY pts = maximumBy (\(Point _ y1 _) (Point _ y2 _) -> y2 `compare` y1) pts

minZ :: [Point] -> Point
minZ pts = maximumBy (\(Point _ _ z1) (Point _ _ z2) -> z2 `compare` z1) pts

doesExceed :: [Point] -> Point -> Bool
doesExceed pts (Point x y z) = x > xp (maxX pts) || y > yp (maxY pts) || z > zp (maxZ pts) || x < xp (minX pts) || y < yp (minY pts) || z < zp (minZ pts)

xp (Point x _ _) = x
yp (Point _ y _) = y
zp (Point _ _ z) = z

-- note to self: need to exclude the face that is requesting
-- ... this still includes an inside chamber with multiple in it
-- maybe iterate through this 1000 times? should catch all chambers (hopefully)?
-- doesn't return whether on outside or not; returns map where true is outside and false is not
-- a lit of all falses causes more recursion, etc...
-- need to return ot top level once iters = some huge number
onOutside :: Side -> Int -> [Side] -> [Point] -> [Side] -> (Bool, Int)
onOutside s 1000 _ _ _ = (False, 1000)
onOutside (Side p f) iters sides points visited = {-(trace $ (if iters == 0 then (=show (Side p f) ++ " " ++ show iters)-} (
    if (Side p f) `elem` sides && iters /= 0 then {-(trace $ show (Side p f) ++ "FALSE")-} (False, iters)
    else if doesExceed points p then {-(trace "TRUE")-} (True, iters) else
    -- dfs is faster
    let pt = (go p f) in {-(trace $ show pt ++ " " ++ show (length visited) ++ " " ++ show (Set.size (Set.fromList visited)))-} (
        if pt `elem` points || pt `elem` (map Day18.pt visited) then {-(trace $ show (Side p f) ++ "FALSE")-} (False, iters)
        else
            let neighbors = [Side pt face | face <- [minBound..maxBound], not ((Side pt face) `elem` visited), face /= oppositeFace f]
                unblocked = (filter (\n -> not (n `elem` sides) && not (n `elem` visited)) neighbors)
                blocked = (filter (\n -> (n `elem` sides)) neighbors)
                numUnblocked = length unblocked in
                    if numUnblocked == 0 then {-(trace $ show (Side p f) ++ "FALSE")-} (False, iters)
                    --else if numUnblocked >= 8 then True -- only need to keep checking if not completely exposed. ** this isn't true, there could be a cube completely exposed then surrounded (e.g. 5x5 hollow) **
                    else 
                        let as = map (\n -> onOutside n (iters+1) sides points ((Side p f):visited)) neighbors
                            truths = map fst as
                            iterses = map snd as in (foldl (||) False truths, iters)))

-- the current version of onOutside returns if a point is on the outside or not, recursing up to tell.
-- we need to put all of the intermediate recursed values in a map to be able to access them in the future.

-- checkOutsideEarlyExit :: [Side] -> Side -> Int -> [Side] -> [Point] -> [Side] -> Map.Map Side Bool -> [(Bool, Map.Map Side Bool)] -> [(Bool, Map.Map Side Bool)]
-- checkOutsideEarlyExit [] _ _ _ _ _ _ l = l
-- checkOutsideEarlyExit (neigh:bors) (Side p f) iters sides points visited m l =
--     let outside = onOutside neigh (iters+1) sides points ((Side p f):visited) m in
--         if fst outside == True then (l ++ [outside])
--         else (checkOutsideEarlyExit bors (Side p f) iters sides points visited m (l ++ [outside]))

-- cyclic dependency becase of the order of the sides.
-- shuffle neighbors?
-- onOutside :: Side -> Int -> [Side] -> [Point] -> [Side] -> Map.Map Side Bool -> (Bool, Map.Map Side Bool)
-- -- onOutside s 1000 _ _ _ m = (False, Map.insert s False m)
-- onOutside (Side p f) iters sides points visited m = (trace $ show (Side p f) ++ " " ++ show iters) (
--     if Map.member (Side p f) m then (m Map.! (Side p f), m)
--     else if (Side p f) `elem` sides && iters /= 0 then {-(trace $ show (Side p f) ++ "FALSE")-} (False, Map.insert (Side p f) False m)
--     else if doesExceed points p then {-(trace "TRUE")-} (True, Map.insert (Side p f) True m) else
--     -- dfs is faster
--     let pt = (go p f) in {-(trace $ show pt ++ " " ++ show (length visited) ++ " " ++ show (Set.size (Set.fromList visited)))-}
--         if pt `elem` points || pt `elem` (map Day18.pt visited) then {-(trace $ show (Side p f) ++ "FALSE")-} (False, Map.insert (Side p f) False m)
--         else
--             let neighbors = [Side pt face | face <- [minBound..maxBound], not ((Side pt face) `elem` visited), face /= oppositeFace f]
--                 unblocked = (filter (\n -> not (n `elem` sides){- && not (n `elem` visited)-}) neighbors)
--                 blocked = (filter (\n -> (n `elem` sides)) neighbors)
--                 numUnblocked = length unblocked in
--                     if numUnblocked == 0 then {-(trace $ show (Side p f) ++ "FALSE")-} (False, Map.insert (Side p f) False m)
--                     --else if numUnblocked >= 8 then True -- only need to keep checking if not completely exposed. ** this isn't true, there could be a cube completely exposed then surrounded (e.g. 5x5 hollow) **
--                     else 
--                         let vals = checkOutsideEarlyExit neighbors (Side p f) iters sides points visited m []
--                             truths = map fst vals
--                             maps = map snd vals
--                             outside = (foldl (||) False truths)
--                             mappy = (Map.insert (Side p f) outside (Map.union (if iters == 0 then m else Map.empty) (Map.unions maps))) in
--                                 {-(trace $ show outside ++ " " ++ show (Map.size mappy))-} (outside, mappy)
--             )

memoize :: (Ord a, Eq a) => (a -> b) -> Map.Map a b -> a -> (b, Map.Map a b)
memoize f m k =
    if Map.member k m then (m Map.! k, m) else let v = f k in (v, Map.insert k v m)

-- need a wrapped version of old onOutside that memoizes

-- tryOutside :: 

-- a point is outside if:
    -- example: 0 5 0, check Y axis
    -- if there is no point 0 (>5) 0 or no point 0 (<5) 0 then it is outside

data Axis = X | Y | Z deriving (Enum, Eq, Ord, Show, Bounded)

pz = zp
py = yp
px = xp

outside :: Side -> [Side] -> Bool
outside (Side (Point x y z) Up) sides =
    let m = maxZ $ filter (\p -> xp p == x && yp p == y) (map pt sides) in z >= (pz m)
outside (Side (Point x y z) Down) sides =
    let m = minZ $ filter (\p -> xp p == x && yp p == y) (map pt sides) in z <= (pz m)
outside (Side (Point x y z) R) sides =
    let m = maxX $ filter (\p -> yp p == y && zp p == z) (map pt sides) in x >= (px m)
outside (Side (Point x y z) L) sides =
    let m = minX $ filter (\p -> yp p == y && zp p == z) (map pt sides) in x <= (px m)
outside (Side (Point x y z) Forward) sides =
    let m = maxY $ filter (\p -> xp p == x && zp p == z) (map pt sides) in y >= (py m)
outside (Side (Point x y z) Back) sides =
    let m = minY $ filter (\p -> xp p == x && zp p == z) (map pt sides) in y <= (py m)

-- chkOutside :: Side -> [Side] -> [Side] -> Bool
-- chkOutside (Side p f) sides visited = 
--     let pt = (go p f)
--         neighbors = [Side pt face | face <- [minBound..maxBound], not ((Side pt face) `elem` visited){-, face /= oppositeFace f-}]
--         isOutside = foldl (\accumulator side -> outside side sides || accumulator) neighbors -- if this is false reiter

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

processOutsides :: [Side] -> [Side] -> [Point] -> Map.Map Side Bool -> Int -> Int
processOutsides [] _ _ _ i = i
processOutsides (x:xs) sides points m i = (trace $ show x) (
    if x `Map.member` m then (trace "Memoized") processOutsides xs sides points m (i + (if m Map.! x then 1 else 0))
    else
        let (outside, iters_) = onOutside x 0 sides points [] in
            (trace $ show x ++ " " ++ show outside ++ " " ++ show iters_) processOutsides xs sides points (Map.insert x outside m) (i + (if outside then 1 else 0)))

-- processOutsides' :: [Side] -> [Side] -> [Point] -> Map.Map Side Bool -> Map.Map Side Bool
-- processOutsides' [] _ _ m = m
-- processOutsides' (x:xs) sides points m = (trace $ show (length xs) ++ " " ++ show x) (
--     let newMap = Map.union m (snd $ onOutside x 0 sides points [] m) in (trace $ show (Map.size newMap) ++ " " ++ show x) processOutsides' xs sides points newMap)

-- part2 = do
--     lines <- getLines "day18/input.txt"
--     let cubes = map parseCube lines
--     let unique = addCubes cubes
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) Down) 0 unique []
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) Up) 0 unique []
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) L) 0 unique []
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) R) 0 unique []
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) Forward) 0 unique []
--     -- print $ onOutside (Side (Point 0 (-59) (-7)) Back) 0 unique []
--     -- print $ onOutside (Side (Point 3 10 5) Down) 0 unique cubes []
--     -- print $ onOutside (Side (Point 3 10 5) Up) 0 unique cubes []
--     -- print $ onOutside (Side (Point 3 10 5) L) 0 unique cubes []
--     -- print $ onOutside (Side (Point 3 10 5) R) 0 unique cubes []
--     -- print $ onOutside (Side (Point 3 10 10) Up) 0 unique cubes []
--     -- print $ onOutside (Side (Point 11 6 9) Up) 0 unique cubes [] Map.empty
--     -- let outsides = processOutsides' unique unique cubes Map.empty
--     -- print $ length $ Map.filter (== True) outsides
--     print $ processOutsides unique unique cubes (Map.fromList [(Side (Point 3 10 10) Up, False), (Side (Point 2 10 11) R, False), (Side (Point 3 9 11) Forward, False), (Side (Point 3 10 12) Down, False)]) 0
--     -- let out = foldl (+) 0 (map (\side -> if onOutside side 0 unique cubes [] then 1 else 0) unique)
--     -- print out

-- New strategy, just using cubes:
-- BFS from highest corner
-- If neighbor is out of bounds, ignore it
-- If neighbor touches

-- BFS is:
-- Initialize with frontier=[largest], visited=[]
-- Get latest from frontier
-- If it is in the cube list, exit and add one to outside count
-- Add to visited
-- Find neighbors
-- Add neighbors not in visited to frontier
-- Recurse with frontier
-- Frontier is FIFO

-- 1663 is wrong :(
-- 4644... is too high...

exceedsCube :: [Point] -> Point -> Bool
exceedsCube pts (Point x y z) = ((x-1) > xp (maxX pts) || (y-1) > yp (maxY pts) || (z-1) > zp (maxZ pts)) || ((1+x) < xp (minX pts) || (1+y) < yp (minY pts) || (1+z) < zp (minZ pts))

neighborsOfCube :: Point -> [Point] -> [Point]
neighborsOfCube c cubes =
    let neighs = [go c face | face <- [minBound..maxBound]] in filter (not . exceedsCube cubes) neighs

sideNeighs :: Point -> [Point] -> [Side]
sideNeighs c cubes =
    let neighs = [Side c face | face <- [minBound..maxBound]] in filter (not . exceedsCube cubes . pt) neighs

-- counting **sides** not **cubes**
-- bfs :: [Point] -> [Point] -> [Point] -> Int -> Int
-- bfs _ [] _ count = count
-- bfs cubes (latest:frontier) visited count = (traceShow latest) (
--     if latest `elem` cubes then bfs cubes frontier (latest:visited) (count+1)
--     else if latest `elem` visited then bfs cubes frontier visited count
--     else
--         let newVisited = latest:visited
--             neighbors = neighborsOfCube latest cubes
--             newNeighbors = filter (`notElem` visited) neighbors
--             newFrontier = frontier ++ newNeighbors
--             in bfs cubes newFrontier newVisited count)
bfs :: [Point] -> [Side] -> Set.Set Side -> Int -> Int
bfs _ [] _ count = count
bfs cubes (latest:frontier) visited count = {-(traceShow visited)-} (
    if latest `elem` visited then bfs cubes frontier visited count
    else if (go (pt latest) (face latest)){-(pt latest)-} `elem` cubes then {-(traceShow latest)-} (bfs cubes frontier (Set.insert latest visited) (count+1))
    else
        let newVisited = (Set.insert latest visited)
            neighbors = sideNeighs (go (pt latest) (face latest)){-(pt latest)-} cubes
            newNeighbors = filter (`Set.notMember` visited) neighbors
            newFrontier = frontier ++ newNeighbors
            in (bfs cubes newFrontier newVisited count))

part2 = do
    lines <- getLines "day18/input.txt"
    let cubes = map parseCube lines
    print $ bfs cubes [Side (Point (px $ maxX cubes) (py $ maxY cubes) (pz $ maxZ cubes)) Up] Set.empty 0