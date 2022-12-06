module Day6 where

import Utilities

-- Function to determine whether all elements of a list are unique
isUnique :: (Eq a) => [a] -> Bool
isUnique [b] = True -- If there's only one item left, it's unique because it hasn't returned false yet.
isUnique list = -- If there are multiple items, it's a bit more complicated
    if (head list) `elem` (tail list) then False -- If the head of the list is within the tail of the list, it's not unique.
    -- But that doesn't cover the entire list!
    else isUnique (drop 1 list) -- I know, call this function again on the list without the first item

-- Example:
-- 1. isUnique abcd -- goes to else condition and calls isUnique again
-- 2. isUnique bcd -- goes to else condition and calls isUnique again
-- 3. isUnique cd -- goes to else condition and calls isUnique again
-- 4. isUnique d -- only one item so returns True

-- Negative examples:
-- 1. isUnique fgfh -- head ('f') is contained in tail ('gfh') so it returns False
--                                                        ^

-- 1. isUnique ghiji -- goes to else condition and calls isUnique again
-- 2. isUnique hiji -- goes to else condition and calls isUnique again
-- 3. isUnique iji -- head ('i') is contained in tail ('iji') so it returns False

{-
This is the standard implementation of the above function, which I didn't know
about, so I wrote my own. It looks like it works a bit differently.

hasNoDups :: (Eq a) => [a] -> Bool

hasNoDups xs = f [] xs
  where
    f _           []     = True
    f seen_so_far (x:xs) = if x `is_elem` seen_so_far
                           then False
                           else f (x:seen_so_far) xs

    is_elem = isIn "hasNoDups"

I think they run at about the same speed -- the cons operator/:, head, and tail
are always fast according to https://wiki.haskell.org/How_to_work_on_lists#Fast_operations.
drop n list gets slower as n gets bigger, but n is always one, so I think it's fine.
I guess the standard implementation might be a bit faster because it's running
`elem` on the list of things it's seen so far, not the entire list?

-}

-- Parameters: <how long chunks are> <list> <index of start of current chunk>
-- returns the index of the end of the first unique chunk chunk + 1
findFirstUniqueIndex' :: (Eq a) => Int -> [a] -> Int -> Int
-- If the list is empty, we didn't find a chunk. Return -1
findFirstUniqueIndex' n [] i = -1
-- Otherwise,
findFirstUniqueIndex' n list i =
    if length list < n then -1 -- not enough items left to make a chunk
    else
        if isUnique $ take n list then i + n -- If the current chunk of the list (taken from the start)
        -- is unique, then return the index of the start of it plus the length of the chunk
        -- (aka the index of the end + 1)
        else findFirstUniqueIndex' n (drop 1 list) (i + 1) -- Otherwise, call this function again with
        -- the first item dropped and the index incremented.

-- This is a wrapper function for findFirstUniqueIndex' that just passes 0 as
-- the third argument.
findFirstUniqueIndex :: (Eq a) => Int -> [a] -> Int
findFirstUniqueIndex n list = findFirstUniqueIndex' n list 0

-- Part 1 -- need to find the first chunk of 4 unique characters
part1 = do
    lines <- getLines "day6/input.txt" -- read input
    if length lines < 1 then putStrLn "Invalid input!" -- if less than 1 line, it's invalid
    else print $ findFirstUniqueIndex 4 (lines !! 0) -- otherwise, print the first unique
    -- chunk of 4 characters long in the first line

-- Part 2 -- need to find the first chunk of 14 unique characters
part2 = do
    lines <- getLines "day6/long_input.txt" -- read input
    if length lines < 1 then putStrLn "Invalid input!" -- if less than 1 line, it's invalid
    else print $ findFirstUniqueIndex 14 (lines !! 0) -- otherwise, print the first unique
    -- chunk of 14 characters long in the first line