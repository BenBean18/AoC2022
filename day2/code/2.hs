-- Import our utilities library
import Utilities

-- Convert ABC to 123 (RPS)
convertToRPS :: Char -> Int
convertToRPS c =
    if c == 'A' then 1
    else if c == 'B' then 2
    else if c == 'C' then 3
    else -1 -- error

-- Input: line, output: (them, desiredOutcome)
parseRPS :: String -> (Int, Char)
parseRPS s = (convertToRPS (s !! 0), s !! 2)

-- Input: them. Output: what you need to do to tie
moveTie :: Int -> Int
moveTie them = them

-- Input: them. Output: what you need to do to win
moveWin :: Int -> Int
moveWin them = 
    if them == 1 then 2
    else if them == 2 then 3
    else if them == 3 then 1
    else -1 -- error

-- Input: them. Output: what you need to do to lose
moveLoss :: Int -> Int
moveLoss them = 
    if them == 2 then 1
    else if them == 3 then 2
    else if them == 1 then 3
    else -1 -- error

-- Input: (them, desiredOutcome). Output: what we need to do to get that outcome (X = lose, Y = tie, Z = win).
getMove :: (Int, Char) -> Int
getMove tuple =
    let them = fst tuple
        desiredOutcome = snd tuple in
    if desiredOutcome == 'X' then (moveLoss them)
    else if desiredOutcome == 'Y' then (moveTie them)
    else (moveWin them)

-- Takes in (them, us) and outputs who won. 6 if we won, 3 if a tie, and 0 if we lost.
getOutcome :: (Int, Int) -> Int
getOutcome tuple =
    let them = fst tuple
        us = snd tuple in
            if us == them then 3
            else if us == 1 && them == 2 then 0
            else if us == 2 && them == 1 then 6
            else if us == 1 && them == 3 then 6
            else if us == 3 && them == 1 then 0
            else if us == 2 && them == 3 then 0
            else if us == 3 && them == 2 then 6
            else -1 -- error!

-- Compute the score of the round. For example, "A Y" prints 4
computeScore :: String -> Int
computeScore s = 
    let round = parseRPS s
        moves = (fst round, getMove round) in
    getOutcome moves + snd moves

-- Do stuff
main = do
    lines <- getLines "input.txt"
    print (sum (map computeScore lines))