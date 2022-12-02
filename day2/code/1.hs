-- Import our utilities library
import Utilities

-- Convert ABC or XYZ to 123 (RPS)
convertToRPS :: Char -> Int
convertToRPS c =
    if c == 'A' || c == 'X' then 1
    else if c == 'B' || c == 'Y' then 2
    else if c == 'C' || c == 'Z' then 3
    else 0

-- Input: line, output: (them, us)
parseRPS :: String -> (Int, Int)
parseRPS s = (convertToRPS (s !! 0), convertToRPS (s !! 2))

-- Takes in (Int, Int) from parseRPS and outputs who won. 6 if we won, 3 if a tie, and 0 if we lost.
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

-- Compute the score for a round. For example, "A Y" prints 8
computeScore :: String -> Int
computeScore s = let round = parseRPS s in getOutcome round + snd round

main = do
    lines <- getLines "input.txt"
    print (sum (map computeScore lines))