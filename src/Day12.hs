module Day12 where

import Utilities
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import Data.PSQueue
import System.Environment
import Criterion.Main

-- I smell Dijkstra's algorithm

data Graph a = Graph { nodes :: [a], neighbors :: Map.Map a [a] } deriving (Eq, Show)
data Node = Node (Int, Int) Char deriving (Eq, Ord, Show)

addTuple :: (Num a) => (a, a) -> (a, a) -> (a, a)
addTuple (a, b) (c, d) = (a+c, b+d)

combineGraphNodes :: Graph a -> Graph a -> Graph a
combineGraphNodes Graph { nodes = g1 } Graph { nodes = g2 } = Graph { nodes = g1 ++ g2, neighbors = Map.empty }

getNodeMap :: [String] -> Map.Map (Int, Int) Char
getNodeMap input = 
    Map.fromList [((x,y),((input !! y) !! x)) | y <- [0..((length input)-1)],
                                                x <- [0..(length (head input) - 1)] ]

getNeighbors' :: Node -> Map.Map (Int, Int) Char -> [Node] -> Int -> Map.Map Node [Node]
getNeighbors' (Node t c) nodeMap currentNeighbors 4 = Map.fromList [((Node t c), currentNeighbors)]
getNeighbors' (Node t c) nodeMap currentNeighbors posIndex =
    let positionsToCheck = [(1,0),(-1,0),(0,1),(0,-1)]
        currentNodePos = (addTuple t (positionsToCheck !! posIndex))
        currentNodeChar_ = nodeMap Map.!? currentNodePos in
        if isNothing currentNodeChar_ {- edge -} then getNeighbors' (Node t c) nodeMap currentNeighbors (posIndex+1)
        else let currentNodeChar = fromJust currentNodeChar_ in
            if (ord (if currentNodeChar == 'S' then 'a' else if currentNodeChar == 'E' then 'z' else currentNodeChar)) <= (ord (if c == 'S' then 'a' else if c == 'E' then 'z' else c))+1 then 
                getNeighbors' (Node t c) nodeMap ((Node currentNodePos currentNodeChar) : currentNeighbors) (posIndex+1)
            else
                getNeighbors' (Node t c) nodeMap currentNeighbors (posIndex+1)

getGraph :: [String] -> Graph Node
getGraph lines =
    let nodeMap = getNodeMap lines
        nodes = map (\((x, y), c) -> Node (x, y) c) (Map.toList nodeMap)
        neighbors = Prelude.foldl Map.union Map.empty (map (\n -> getNeighbors' n nodeMap [] 0) nodes) in
            Graph { nodes = nodes, neighbors = neighbors }

-- just need breadth first search here
-- has a queue of nodes to visit
-- pop a node out of the queue
-- for each of its neighbors,
-- add it to the queue if we haven't visited it
-- and Map.insert thisNode neighbor
-- i'll actually do dijkstra in case part 2 involves movement costs
-- thanks to https://www.redblobgames.com/pathfinding/a-star/introduction.html
dijkstra' :: Graph Node -> PSQ Node Int -> Map.Map Node Node -> Map.Map Node Int -> Node -> (Map.Map Node Node, Map.Map Node Int)
dijkstra' Graph { nodes = nodes, neighbors = neighbors } pq cameFrom costMap goal =
    let currentNode_ = findMin pq
        newPQ_ = deleteMin pq in
        if isNothing currentNode_ then (cameFrom, costMap)
        else
            let currentNode = key $ fromJust currentNode_ in
                if currentNode == goal then 
                    (cameFrom, costMap)
                else
                    let (newPQ, newCameFrom, newCostMap) = handleNeighbors newPQ_ cameFrom costMap currentNode (neighbors Map.! currentNode) in
                        dijkstra' Graph { nodes = nodes, neighbors = neighbors } newPQ newCameFrom newCostMap goal

getStart :: Graph Node -> Node
getStart Graph { nodes = nodes } = head (filter (\(Node pos c) -> c == 'S') nodes)

getEnd :: Graph Node -> Node
getEnd Graph { nodes = nodes } = head (filter (\(Node pos c) -> c == 'E') nodes)

dijkstra :: Graph Node -> Node -> (Map.Map Node Node, Map.Map Node Int)
dijkstra g goal = 
    dijkstra' g (singleton (getStart g) 0) Map.empty (Map.singleton (getStart g) 0) goal

handleNeighbors :: PSQ Node Int -> Map.Map Node Node -> Map.Map Node Int -> Node -> [Node] -> (PSQ Node Int, Map.Map Node Node, Map.Map Node Int)
handleNeighbors pq cameFrom costMap origNode [] = (pq, cameFrom, costMap)
handleNeighbors pq cameFrom costMap origNode neighbors =
    let newCost = costMap Map.! origNode + 1
        neighbor = head neighbors in
            if isNothing (costMap Map.!? neighbor) {- we haven't visited yet -} then
                let newCostMap = Map.insert neighbor newCost costMap
                    newPQ = insert neighbor newCost pq {- cost = priority so we find lowest cost -}
                    newCameFrom = Map.insert neighbor origNode cameFrom in
                        handleNeighbors newPQ newCameFrom newCostMap origNode (tail neighbors)
            else
                let shortestCurrentCost = costMap Map.! neighbor in
                    if newCost < shortestCurrentCost then
                        let newCostMap = Map.insert neighbor newCost costMap
                            newPQ = insert neighbor newCost pq {- cost = priority so we find lowest cost -}
                            newCameFrom = Map.insert neighbor origNode cameFrom in
                                handleNeighbors newPQ newCameFrom newCostMap origNode (tail neighbors)
                    else handleNeighbors pq cameFrom costMap origNode (tail neighbors)

-- followBack :: Node -> Map.Map Node Node -> Node -> [Node] -> [Node]
-- followBack first cameFrom first l = l
-- followBack last cameFrom first l = let newNode = cameFrom Map.! last in
--     followBack newNode cameFrom first (newNode:l)

-- 457 too high (assumed start was always the first node, which wasn't true)

part1' lines = do
    let graph = getGraph lines
    let end = getEnd graph
    let (cameFrom, costMap) = dijkstra graph end
    print $ costMap Map.! end

-- Part 2

-- inverted neighbors since we're going the other way
getNeighbors2' :: Node -> Map.Map (Int, Int) Char -> [Node] -> Int -> Map.Map Node [Node]
getNeighbors2' (Node t c) nodeMap currentNeighbors 4 = Map.fromList [((Node t c), currentNeighbors)]
getNeighbors2' (Node t c) nodeMap currentNeighbors posIndex =
    let positionsToCheck = [(1,0),(-1,0),(0,1),(0,-1)]
        currentNodePos = (addTuple t (positionsToCheck !! posIndex))
        currentNodeChar_ = nodeMap Map.!? currentNodePos in
        if isNothing currentNodeChar_ {- edge -} then getNeighbors2' (Node t c) nodeMap currentNeighbors (posIndex+1)
        else let currentNodeChar = fromJust currentNodeChar_ in
            if (ord (if currentNodeChar == 'S' then 'a' else if currentNodeChar == 'E' then 'z' else currentNodeChar))+1 >= (ord (if c == 'S' then 'a' else if c == 'E' then 'z' else c)) then 
                getNeighbors2' (Node t c) nodeMap ((Node currentNodePos currentNodeChar) : currentNeighbors) (posIndex+1)
            else
                getNeighbors2' (Node t c) nodeMap currentNeighbors (posIndex+1)

getGraph2 :: [String] -> Graph Node
getGraph2 lines =
    let nodeMap = getNodeMap lines
        nodes = map (\((x, y), c) -> Node (x, y) c) (Map.toList nodeMap)
        neighbors = Prelude.foldl Map.union Map.empty (map (\n -> getNeighbors2' n nodeMap [] 0) nodes) in
            Graph { nodes = nodes, neighbors = neighbors }

dijkstra2' :: Graph Node -> PSQ Node Int -> Map.Map Node Node -> Map.Map Node Int -> (Node -> Bool) -> (Map.Map Node Node, Map.Map Node Int, Node)
dijkstra2' Graph { nodes = nodes, neighbors = neighbors } pq cameFrom costMap goalCondition =
    let currentNode_ = findMin pq
        newPQ_ = deleteMin pq in
        if isNothing currentNode_ then (cameFrom, costMap, (Node (-1, -1) ':'))
        else
            let currentNode = key $ fromJust currentNode_ in
                if goalCondition currentNode then 
                    (cameFrom, costMap, currentNode)
                else
                    let (newPQ, newCameFrom, newCostMap) = handleNeighbors newPQ_ cameFrom costMap currentNode (neighbors Map.! currentNode) in
                        dijkstra2' Graph { nodes = nodes, neighbors = neighbors } newPQ newCameFrom newCostMap goalCondition


dijkstra2 :: Graph Node -> (Map.Map Node Node, Map.Map Node Int, Node)
dijkstra2 g = 
    dijkstra2' g (singleton (getEnd g) 0) Map.empty (Map.singleton (getEnd g) 0) (\(Node pos c) -> c == 'a' || c == 'S')

part2' lines = do
    let graph = getGraph2 lines
    let (cameFrom, costMap, start) = dijkstra2 graph
    print $ costMap Map.! start

-- Benchmarking
part1 = do
    lines <- getLines "day12/input.txt"
    part1' lines

part2 = do
    lines <- getLines "day12/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day12.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day12/input.txt"
    time lines