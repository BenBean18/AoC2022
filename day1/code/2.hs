-- Read a file line by line

import System.IO
import Text.Read
import Data.Maybe
import Debug.Trace
import Data.List
import Utilities

toInt :: String -> Maybe Int
toInt i = readMaybe i :: Maybe Int

debug = flip trace

-- Takes in a list of numbers separated by Nothings. Adds them up. Outputs the sums in the 3rd parameter which is a list of Ints.
addAndReturnList :: [Maybe Int] -> Int -> [Int] -> ([Maybe Int], Int, [Int])
addAndReturnList list sum elves =
    -- if we've added all things in the list,
    if length list == 0
        then 
            -- stop recursing and return the result
            (list, sum, elves)
        else
            -- we've got stuffs to add!
            let f = head list in
                -- f = first item of list
                if f == Nothing
                    then
                        -- if f is Nothing (separator), then add the sum to the list of elves and recurse
                        addAndReturnList (tail list) 0 (sum : elves)
                    else
                        -- otherwise, add the first item in the list to the sum and recurse
                        addAndReturnList (tail list) (sum + fromJust f) elves

for list action = mapM_ action list

rec_sum []     = 0
rec_sum (x:xs) = x + rec_sum xs

main = do
    out <- getInts "input.txt"
    -- call addAndReturnList on the ints
    let (list, sum, elves) = addAndReturnList out 0 []
    -- sort the returned elves
    let sortedElves = sort elves
    -- reverse the elves
    let revElves = reverse sortedElves
    -- get largest 3 elves
    let largest3 = take 3 revElves
    -- print the sum of the largest 3 elves (first 3 of reversed)
    let sumOfLargest3 = rec_sum largest3
    print sumOfLargest3