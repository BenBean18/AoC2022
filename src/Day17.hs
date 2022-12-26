module Day17 where

import Utilities
import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe
import qualified Day17_part2_actual

-- tetris :)

type Rock = [[Bool]]

-- ####

-- .#.
-- ###
-- .#.

-- ..#
-- ..#
-- ###

-- #
-- #
-- #
-- #

-- ##
-- ##

andTuple :: (Bool, Bool) -> Bool
andTuple (a, b) = a && b

andList :: [Bool] -> [Bool] -> [Bool]
andList aL bL = map andTuple (zip aL bL)

orTuple :: (Bool, Bool) -> Bool
orTuple (a, b) = a || b

orList :: [Bool] -> [Bool] -> [Bool]
orList aL bL = (map orTuple (zip aL bL))

doesCollide :: [Bool] -> [Bool] -> Bool
doesCollide a b = (<) 0 $ length $ filter (==True) (andList a b)

getRock :: Int -> Rock
getRock 0 = [[True,True,True,True]]
getRock 1 = [[False,True,False],
             [True,True,True],
             [False,True,False]]
getRock 2 = [[False,False,True],
             [False,False,True],
             [True,True,True]]
getRock 3 = [[True],
             [True],
             [True],
             [True]]
getRock 4 = [[True,True],
             [True,True]]

rockHeight rock = length rock
rockWidth rock = length $ head rock
bottomRow rock = last rock

type Board = Map.Map Int [Bool]
type Move = Char
data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)
-- ...x7 (ROWSxCOLUMNS)
-- Each rock appears so that its left edge is two units away from the left wall and its bottom edge is three units above the highest rock in the room (or the floor, if there isn't one).
-- so keep track of the left bottom

-- increase of y = up

{-
...
6
5
4
3
2
1
0
-}

-- need collision/combination function
-- should be ok to store the entire array, maximum is 36800

-- board should be initialized where 

addRockToBoard :: Board -> Rock -> Point -> Board
addRockToBoard board [] _ = board
addRockToBoard board rock Point { x = x_, y = y_ } =
    let oldRock = (take (rockWidth rock) (drop x_ (getRow board y_)))
        oldPrefix = (take x_ (getRow board y_))
        oldSuffix = (drop (x_ + rockWidth rock) (getRow board y_)) in
    addRockToBoard (Map.insert y_ (oldPrefix ++ (orList (bottomRow rock) oldRock) ++ oldSuffix) board) (init rock) Point { x = x_, y = y_ + 1 }

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

clamp a b v = if v < a then a else if v > b then b else v

getRow :: Board -> Int -> [Bool]
getRow b y =
    let val = b Map.!? y in
        if isNothing val then [False,False,False,False,False,False,False] else fromJust val

-- it's not just the bottom!
-- e.g.
{-
BBB
ABA
 B
-}

showBoolList :: [Bool] -> String
showBoolList bools = map (\b -> if b then '#' else '.') bools

showBoard :: Board -> Int -> String -> String
showBoard _ (-1) s = s
showBoard board height s = showBoard board (height-1) $ s ++ showBoolList (getRow board height) ++ "\n"

showRock :: Rock -> String -> String
showRock [] s = s
showRock rock s = showRock (tail rock) $ s ++ showBoolList (head rock) ++ "\n"

-- move is > or < or v
-- returns the board and the top of the rock
doMove :: Board -> Rock -> [Move] -> Move -> Point -> (Board, Int, [Move])
doMove board rock moves move Point { x = x_, y = y_ } = {-(trace $ if move /= 'v' then move:[] else "")-} (
    let newPosition_ = Point { x = x_ + if move == '>' then 1 else if move == '<' then -1 else 0, y = y_ - if move == 'v' then 1 else 0 }
        newPosition = Point { x = (clamp 0 (7 - rockWidth rock) (x newPosition_)), y = (y newPosition_) }
        boardSection = map (\row -> take (rockWidth rock) $ drop (x newPosition) row) (foldl (\a x -> a ++ [x]) [] (map (getRow board) $ reverse [(y newPosition)..(y newPosition + rockHeight rock - 1)]))
            in
            if (foldl (||) False (map (\(a, b) -> doesCollide a b) (zip boardSection rock)) || y newPosition <= -1) && (move == 'v') then {-(trace $ showRock rock "" ++ "\n" ++ showRock boardSection "")-} (addRockToBoard board rock Point { x = x_, y = y_ }, y_ + rockHeight rock, moves)
            else if (foldl (||) False (map (\(a, b) -> doesCollide a b) (zip boardSection rock))) then doMove board rock moves 'v' Point { x = x_, y = y_ }
            else
                if move /= 'v' then doMove board rock moves 'v' newPosition
                else doMove board rock (tail moves) (head moves) newPosition)

doMoves :: (Board, Int, [Move]) -> [Move] -> Int -> Int -> (Board, Int)
doMoves (b,h,_) _ _ 0 = (b,h)
doMoves (b,h,moves) origMoves rockIndex rockNum = {-(trace $ showBoard b h "")-} (
    let currentMoveList = moves
        newMoveList = if length currentMoveList < 10 then currentMoveList ++ origMoves else currentMoveList
        (newB,newH,newMoves) = (doMove b (getRock rockIndex) (tail newMoveList) (head newMoveList) Point { x = 2, y = h+3 }) in
    doMoves (newB, max newH h, newMoves) origMoves ((rockIndex + 1) `mod` 5) (rockNum-1))

-- recursive function, operates using head of input and calls itself on tail
-- returns when 2022 high

part1 = do
    lines <- getLines "day17/input.txt"
    let moves = head lines
    let b_ = Map.empty
    let (b, height) = doMoves (b_,0,moves) moves 0 2022
    print height

part2 = Day17_part2_actual.part2