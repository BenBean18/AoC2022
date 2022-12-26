module Day17_part2_actual where

import Utilities
import Debug.Trace
import qualified Data.Map as Map
import Data.Maybe
import Data.List (transpose)

-- tetris :)

type Rock = [[Bool]]

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

data Board = Board [[Bool]] Int
type Move = Char
data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)

-- Storing the top 1000 should be ok.

addRockToBoard :: Board -> Rock -> Point -> Board
addRockToBoard board [] _ = board
addRockToBoard board rock Point { x = x_, y = y_ } =
    let oldRock = (take (rockWidth rock) (drop x_ (getRow board y_)))
        oldPrefix = (take x_ (getRow board y_))
        oldSuffix = (drop (x_ + rockWidth rock) (getRow board y_)) in
    addRockToBoard (setRow board y_ (oldPrefix ++ (orList (bottomRow rock) oldRock) ++ oldSuffix)) (init rock) Point { x = x_, y = y_ + 1 }

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

clamp a b v = if v < a then a else if v > b then b else v

-- hopefully this is only called on length + 1 or below...
setRow :: Board -> Int -> [Bool] -> Board
setRow (Board bl h) y row =
    let i = (y - h) in
        if i == length bl then Board ((tail bl) ++ [row]) (h+1)
        else if i > length bl then error "sadness"
        else Board (set1D i row bl) h

getRow :: Board -> Int -> [Bool]
getRow (Board bl h) (-1) = [False,False,False,False,False,False,False]
getRow (Board bl h) y =
    let i = (y - h)
        val = if i < length bl then bl !! i else [False,False,False,False,False,False,False] in val

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
            if (foldl (||) False (map (\(a, b) -> doesCollide a b) (zip boardSection rock)) || y newPosition <= -1) && (move == 'v') then {-(trace $ showRock rock "" ++ "\n" ++ showRock boardSection "")-} (($!) addRockToBoard board rock Point { x = x_, y = y_ }, y_ + rockHeight rock, moves)
            else if (foldl (||) False (map (\(a, b) -> doesCollide a b) (zip boardSection rock))) then doMove board rock moves 'v' Point { x = x_, y = y_ }
            else
                if move /= 'v' then doMove board rock moves 'v' newPosition
                else doMove board rock (tail moves) (head moves) newPosition)


backRock 0 = 4
backRock n = (n-1) `mod` 5

doMoves :: (Board, Int, [Move]) -> [Move] -> Int -> Int -> (Board, Int, Rock)
doMoves (b,h,_) _ rIdx 0 = (b,h,getRock (backRock rIdx))
doMoves (b,h,moves) origMoves rockIndex rockNum = {-(traceShow $ boardDiff b)-} (
    let currentMoveList = moves
        newMoveList = if length currentMoveList < 10 then currentMoveList ++ origMoves else currentMoveList
        (newB,newH,newMoves) = (doMove b (getRock rockIndex) (tail newMoveList) (head newMoveList) Point { x = 2, y = h+3 }) in
    doMoves (newB, max newH h, newMoves) origMoves ((rockIndex + 1) `mod` 5) (rockNum-1))

-- recursive function, operates using head of input and calls itself on tail
-- returns when 2022 high

maximumIndex' :: [Bool] -> Int -> Int
maximumIndex' [] _ = 0
maximumIndex' bools idx = 
    if last bools then idx else maximumIndex' (init bools) (idx-1)

maximumIndex bools = maximumIndex' bools (length bools - 1)

boardDiff :: Board -> [Int]
boardDiff (Board bools _) =
    let ints = map maximumIndex $ transpose bools in (map (\i -> i - (minimum ints)) ints)

emptyBoard :: Board -> Int -> Board
emptyBoard b 0 = b
emptyBoard (Board bl _) i = emptyBoard (Board ([False,False,False,False,False,False,False]:bl) 0) (i-1)

getSize (Board l _) = length l
getHeight (Board _ h) = h

-- is this linear approximation?

getHeightOfRocks :: Int -> [Move] -> Int
getHeightOfRocks m moves = 
    let b_ = emptyBoard (Board [] 0) 100 -- 100 high
        (_, height, _) = ($!) doMoves (b_,0,moves) moves 0 m in height

allMoves :: (Board, [Int], Int, [Move]) -> [Move] -> Int -> Int -> (Board, [Int], Rock)
allMoves (b,hs,h,_) _ rIdx 0 = (b,hs,getRock (backRock rIdx))
allMoves (b,hs,h,moves) origMoves rockIndex rockNum = {-(traceShow $ boardDiff b)-} (
    let currentMoveList = moves
        newMoveList = if length currentMoveList < 10 then currentMoveList ++ origMoves else currentMoveList
        (newB,newH,newMoves) = (doMove b (getRock rockIndex) (tail newMoveList) (head newMoveList) Point { x = 2, y = h+3 }) in
    allMoves (newB, newH:hs, max newH h, newMoves) origMoves ((rockIndex + 1) `mod` 5) (rockNum-1))

-- map: index = number of rocks, value = height
doesAPatternExist' :: Int -> Int -> [Int] -> Int
doesAPatternExist' difference current l =
    let size = length l in
        if current >= size then -1
        else if current + difference >= size then (l !! (current) - l !! (current - difference))
        else
            let diffHeight = (l !! (current + difference) - l !! current) in
                    if diffHeight == doesAPatternExist' difference (current + difference) l then diffHeight else -1

doesAPatternExist difference l = doesAPatternExist' difference difference l

-- returns (rock difference, height difference)
findPattern' :: Int -> [Int] -> (Int, Int)
findPattern' i l =
    if length l <= i * 2 then (-1, -1)
    else
        let diff = doesAPatternExist i l in
            if diff == -1 then findPattern' (i+1) l
            else (i, diff)

findPattern m = findPattern' 1 m

-- See the bottom of this spreadsheet for my logic:
-- https://docs.google.com/spreadsheets/d/10m9ZiCsxk_1mgQaOws1UkGjhU-9_88d_tVBFFOngnPI/edit

part2 = do
    lines <- getLines "day17/input.txt"
    let moves = head lines
    let b_ = emptyBoard (Board [] 0) 100 -- 100 high
    let iters = 10000 -- increasing this leads to a higher probability of a
    -- correct solution in exchange for a longer runtime. 10000 gave me the
    -- right answer.
    let (b, heights_, rock) = ($!) allMoves (b_,[],0,moves) moves 0 iters
    print $ head heights_
    let heights = reverse heights_
    let (rockDiff, heightDiff) = findPattern heights
    print (rockDiff, heightDiff)
    if rockDiff == -1 then putStrLn "No pattern found :(\nTry making iters bigger."
    else do
        let startRockNum = 1000000000000 `mod` rockDiff
        let numberOfDiffs = 1000000000000 `div` rockDiff -- div rounds down which is what we want
        let heightToAdd = numberOfDiffs * heightDiff
        let initialHeight = heights !! startRockNum
        putStrLn $ show $ initialHeight + heightToAdd - 1 -- not sure why there is an off by one error here but -1 works...