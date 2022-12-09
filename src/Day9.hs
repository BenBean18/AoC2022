module Day9 where

import Utilities
import qualified Data.Set as Set

-- I feel like there's a better way to do this than brute force it but I don't know how
-- I guess using a set instead of storing the entire 2D array helps some?

data Point = Point Int Int deriving (Eq, Ord, Show)
data BridgeState = BridgeState { h :: Point, t :: Point, tailVisited :: Set.Set Point } deriving (Eq, Show)

isCloseEnough :: BridgeState -> Bool
isCloseEnough BridgeState {h=(Point hx hy),t=(Point tx ty)} = abs(hx-tx) <= 1 && abs(hy-ty) <= 1

pointPlus :: Point -> Point -> Point
pointPlus (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

pointMinus :: Point -> Point -> Point
pointMinus (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

-- Point represents a move (x = right, y = up)
doMove :: BridgeState -> Point -> BridgeState
doMove state (Point 0 0) = state
doMove state (Point x y) = let lastHead = h state
                               xMove = (if x == 0 then 0 else if x > 0 then 1 else -1)
                               yMove = (if y == 0 then 0 else if y > 0 then 1 else -1)
                               movePt = (Point xMove yMove)
                               newHead = lastHead `pointPlus` movePt
                               newHeadState = BridgeState { h = newHead, t = t state, tailVisited = tailVisited state } in
    if isCloseEnough $ BridgeState { h = newHead, t = t state, tailVisited = tailVisited state } then
        doMove newHeadState (Point (x - xMove) (y - yMove))
    else
        doMove BridgeState { h = newHead, t = lastHead, tailVisited = Set.insert lastHead (tailVisited state) } (Point (x-xMove) (y-yMove))

parseMove :: String -> Point
parseMove s = let dir = head s
                  magnitude = read (tail s) :: Int in
    if dir == 'R' then Point magnitude 0
    else if dir == 'L' then Point (-magnitude) 0
    else if dir == 'U' then Point 0 magnitude
    else Point 0 (-magnitude)

part1 = do
    lines <- getLines "day9/input.txt"
    let moves = map parseMove lines
    let finalState = foldl doMove BridgeState { h = Point 0 0, t = Point 0 0, tailVisited = Set.empty } moves
    print $ (length . tailVisited $ finalState) + 1 -- +1 to account for start

-- Part 2, maybe

data BridgeState2 = BridgeState2 { knots :: [Point], tv :: Set.Set Point } deriving (Eq, Show)

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

-- Write a function that moves a knot (with specified index one place).
-- It then checks to see if the next knot is close enough.
-- If it's not, it moves the next one to the original place of the knot.
-- this can only move 1 space, not multiple (0,1) or (0,-1) or (1,0) or (-1,0)

arePointsCloseEnough :: Point -> Point -> Bool
arePointsCloseEnough (Point hx hy) (Point tx ty) = abs(hx-tx) <= 1 && abs(hy-ty) <= 1

-- returns the updated position of point 2
-- updatePoint leader follower
updatePoint :: Point -> Point -> Point
updatePoint (Point x1 y1) (Point x2 y2) =
    if x1 == x2 then
        if y1 > y2 then (Point x2 (y2+1)) else (Point x2 (y2-1))
    else if y1 == y2 then
        if x1 > x2 then (Point (x2+1) y2) else (Point (x2-1) y2)
    else
        if y1 > y2 then
            if x1 > x2 then (Point (x2+1) (y2+1)) else (Point (x2-1) (y2+1))
        else
            if x1 > x2 then (Point (x2+1) (y2-1)) else (Point (x2-1) (y2-1))

moveKnot :: Int -> Point -> BridgeState2 -> BridgeState2
moveKnot index (Point x y) state =
    let lastKnotPos = (knots state) !! index
        xMove = x
        yMove = y
        movePt = (Point xMove yMove)
        newKnot = lastKnotPos `pointPlus` movePt
        nextKnot = (knots state) !! (index + 1)
        newKnotsState = BridgeState2 { knots = set1D index newKnot (knots state), tv = tv state } in
            if arePointsCloseEnough newKnot nextKnot then
                newKnotsState
            else do
                --print lastKnotPos
                if (index + 1) == ((length (knots state)) - 1) then
                    -- tail
                    BridgeState2 { knots = set1D (index + 1) (updatePoint newKnot nextKnot) (knots newKnotsState), tv = Set.insert (updatePoint newKnot nextKnot) (tv state) }
                -- otherwise, move the difference between this one and previous
                else
                    --print $ "TO " ++ show newKnot ++ " FROM " ++ show lastKnotPos ++ " AFTER " ++ show nextKnot
                    --let latestState = BridgeState2 { knots = set1D (index + 1) lastKnotPos (knots newKnotsState), tv = tv state } in
                    moveKnot (index + 1) ((updatePoint newKnot nextKnot) `pointMinus` nextKnot) newKnotsState

-- Arguments: index and list
-- If index and index + 1 are not touching, 

-- redefine isCloseEnough to take two points

doMove2 :: BridgeState2 -> Point -> BridgeState2
doMove2 state (Point 0 0) = state
doMove2 state (Point x y) = let xMove = (if x == 0 then 0 else if x > 0 then 1 else -1)
                                yMove = (if y == 0 then 0 else if y > 0 then 1 else -1)
                                movePt = (Point xMove yMove) in
    doMove2 (moveKnot 0 movePt state) (Point (x-xMove) (y-yMove))

part2 = do
    lines <- getLines "day9/input.txt"
    let moves = map parseMove lines
    let finalState = foldl doMove2 BridgeState2 { knots = [(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0),(Point 0 0)], tv = Set.empty } moves
    print $ (length . tv $ finalState) + 1

-- wow that was heckin hard
-- Z = E+F+G
-- X = F+G
-- my problem was that if you have:
{-

     A
     B
  ZDC

-}
-- then use the last recorded position of the leader to move to (which I thought
-- was a clever solution), it can in some cases move in a line instead of taking
-- the most efficient (and rule following) diagonal
-- Example (A = head, B through G is following, Z = F & G shadowed by E, X = G shadowed by F)
-- A up one
{-
     A
     
     B
  ZDC

-}
-- B updates
{-
     A
     B
     
  ZDC

-}
-- C updates (* is the last position of C)
{-
     A
     B
     C
  ZD*

-}
-- D updates to the last position of C
{-
     A
     B
     C
  XED

-}
-- THIS IS WRONG!!
-- According to the rules outlined in the problem,
-- D should have moved diagonally since it's not on the same row or column
-- The correct position would be this:
-- D updates
{-
     A
     B
    DC
  XE

-}
-- so I defined the function updatePoint to update a point correctly.