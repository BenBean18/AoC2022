module Day5 where

import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List
import Debug.Trace
import System.Environment
import Criterion.Main

debug = flip trace

insert1D :: Int -> a -> [a] -> [a]
insert1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ zs

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

-- we're going to ignore the zeroth index of the crates and start at 1
addCrateStack :: [[Char]] -> String -> [[Char]]
addCrateStack stacks "" = stacks
addCrateStack stacks str = 
    if (str !! 1) /= '1' && ((str !! 0) == ' ' || (str !! 0) == '[') then
        let matches = (str ++ "\n") =~ "(\\[[A-Z]\\]|   )[ \\n]" :: [[String]]
            crateIndex = 9 - ((length (str ++ "\n")) `div` 4) + 1
            crate = ((matches !! 0) !! 1) !! 1 in
        -- each of the matches represents this row of crates
        -- we want to add each existent crate to stacks
        -- i guess use recursion?
        if crate == ' ' then addCrateStack stacks (drop 4 str)
        else addCrateStack (set1D crateIndex ((stacks !! crateIndex) ++ [crate]) stacks) (drop 4 str)
    else stacks

parseInstruction :: [[Char]] -> String -> [[Char]]
parseInstruction stacks str = 
    let matches = str =~ "move ([0-9]+) from ([0-9]+) to ([0-9]+)" :: [[String]] in
        if length matches /= 1 || length (matches !! 0) /= 4 then stacks -- invalid line, probably going through the start of the list
        else 
            let fromIndex = (read ((matches !! 0) !! 2)) :: Int
                count = (read ((matches !! 0) !! 1)) :: Int
                toIndex = (read ((matches !! 0 ) !! 3)) :: Int
                from = reverse (take count (stacks !! fromIndex))
                newColumnFrom = drop count (stacks !! fromIndex)
                newColumnTo = from ++ (stacks !! toIndex)
                in (set1D toIndex newColumnTo (set1D fromIndex newColumnFrom stacks))

-- use folds
part1' lines = do
    let stacks = (foldl parseInstruction (foldl addCrateStack [[],[],[],[],[],[],[],[],[],[]] lines) lines)
    let heads = map head (tail stacks) -- ignore index 0
    print heads

-- Part 2

parseInstruction2 :: [[Char]] -> String -> [[Char]]
parseInstruction2 stacks str = 
    let matches = str =~ "move ([0-9]+) from ([0-9]+) to ([0-9]+)" :: [[String]] in
        if length matches /= 1 || length (matches !! 0) /= 4 then stacks -- invalid line, probably going through the start of the list
        else 
            let fromIndex = (read ((matches !! 0) !! 2)) :: Int
                count = (read ((matches !! 0) !! 1)) :: Int
                toIndex = (read ((matches !! 0 ) !! 3)) :: Int
                from = take count (stacks !! fromIndex) -- not reversed for part 2
                newColumnFrom = drop count (stacks !! fromIndex)
                newColumnTo = from ++ (stacks !! toIndex)
                in (set1D toIndex newColumnTo (set1D fromIndex newColumnFrom stacks))

part2' lines = do
    let stacks = (foldl parseInstruction2 (foldl addCrateStack [[],[],[],[],[],[],[],[],[],[]] lines) lines)
    let heads = map head (tail stacks) -- ignore index 0
    print heads

-- Benchmarking
part1 = do
    lines <- getLines "day5/input.txt"
    part1' lines

part2 = do
    lines <- getLines "day5/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day5.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day5/input.txt"
    time lines