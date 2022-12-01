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

main = do
    out <- getInts "input.txt"
    -- call addAndReturnList on the ints
    let (list, sum, elves) = addAndReturnList out 0 []
    -- sort the returned elves
    let sortedElves = sort elves
    -- print the last elf. this is done by reversing the list and getting the first item (which is the non-reversed last item)
    print (head (reverse sortedElves))
