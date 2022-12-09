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