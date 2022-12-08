module Day7 where

import Utilities
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import Text.Regex.Base
import Text.Regex.PCRE
import Data.Maybe
import Control.Concurrent
import System.IO

-- I think creating the whole directory tree would be inefficient

-- So we're just storing a dictionary<path, size>.
-- When we add a new file, it recurses upward through the tree and adds the
-- size to all of those directories.

-- Custom datatype for filesystem state, has the abovementioned size map &
-- current path
data FSState = FSState { path :: String, sizeMap :: Map.Map String Int }

-- remove the last directory from the path
-- e.g. upOne /ab/cd/efg == /ab/cd
upOne :: FSState -> FSState
upOne fss = 
    let p = '/' : ((path fss) =~ "(([a-z]+)\\/)+" :: String) in
        if p == "/" then FSState { path = p, sizeMap = (sizeMap fss) }
        else FSState { path = init p, sizeMap = (sizeMap fss) }

-- carry out a directory change
parseCd :: FSState -> String -> FSState
parseCd fss cdCmd =
    if cdCmd == ".." then upOne fss -- remove last dir from path and cd to it
    else if cdCmd == "/" then FSState {path = "/", sizeMap = (sizeMap fss)} -- go to /
    else
        -- this avoids a path with // in it which messes everything up
        if (path fss) == "/" then FSState {path = (path fss) ++ (cdCmd), sizeMap = (sizeMap fss)}
        else FSState {path = (path fss) ++ ('/' : cdCmd), sizeMap = (sizeMap fss)}

addSizeToMap :: FSState -> Int -> FSState
addSizeToMap fss size =
    -- I was having a very annoying bug with this function that I was able
    -- to debug by doing putStrLn ((path fss) ++" " ++ (show $ sizeMap fss) ++ " " ++ (head list))
    -- in recurseAccumulate.
    -- This sets the path to / when it is done, so the path is set to / after a size is added.
    -- I switched to only using the map from this function in the main one.
    if (path fss) == "/" then
        -- if we've gotten to the top level return
        let m = Map.Strict.insertWith (+) (path fss) size (sizeMap fss) in FSState {path = (path fss), sizeMap = m}
    else
        -- otherwise add the size to the current directory and add it to
        -- the next one up and so on until we recurse to / and trigger line 49
        let 
            newPath = path (upOne fss)
            m = Map.Strict.insertWith (+) (path fss) size (sizeMap fss)
            in addSizeToMap (FSState {path = newPath, sizeMap = m}) size

-- expects command
runCmd :: FSState -> String -> FSState
runCmd fss out =
    let cmd = take 4 out in -- cmd = first 4 chars
        if cmd == "$ cd" then -- "$ cd" == cd
            let arg = drop 5 out in parseCd fss arg
        else if cmd == "$ ls" then -- "$ ls" == ls
            -- do nothing, we'll handle that when we get the files
            fss
        else if cmd == "dir " then -- will be "dir "
            -- not a command so a dir
            -- we'll just match files since we don't care about directories
            fss
        else
            -- yay file!
            let match = out =~ "(\\d+)" :: String in -- match all digits (the size is a number)
                -- return a new FSState using the current path and the size from addSizeToMap
                FSState { path = (path fss), sizeMap = sizeMap (addSizeToMap fss (read match :: Int)) }

-- print filesystem state (current directory + its size)
printState :: FSState -> IO ()
printState fss = 
    let pathLen = length $ "\r" ++ (path fss)
        pwdSize = fromMaybe 0 (sizeMap fss Map.!? path fss)
        sizeLen  = length (show pwdSize) in do
            putStr $ "\r" ++ (path fss) ++ (replicate (80 - pathLen) ' ') ++ " = " ++ (show pwdSize) ++ " bytes" ++ (replicate (20 - sizeLen) ' ')
            hFlush stdout

recurseAccumulate :: FSState -> [String] -> IO FSState
recurseAccumulate fss [] = 
    do return fss
recurseAccumulate fss list = 
    do
        printState fss
        threadDelay 500000 -- 0.5 seconds
        (recurseAccumulate (runCmd fss (head list)) (drop 1 list))

part1 = do
    lines <- getLines "day7/input.txt"
    -- fold takes a function, list, and initial value. the function takes in
    -- the current value and outputs a new one based on the list item.
    -- fold returns the final value. l means from the left.
    -- recurseAccumulate is basically an implementation of fold
    let fss = foldl runCmd FSState { path = "/", sizeMap = Map.empty } lines
    -- let soln = fold 
    putStrLn $ "Sum of the sizes of all directories under 100000: " ++ (show $ sum $ Map.filter (<= 100000) (sizeMap fss))

-- 24390891 is wrong

part2 = do
    lines <- getLines "day7/input.txt"
    let fss = foldl runCmd FSState { path = "/", sizeMap = Map.empty } lines
    let driveSize = 70000000
    let updateSize = 30000000
    let usedSpace = ((sizeMap fss) Map.! "/")
    let unusedSpace = driveSize - usedSpace
    let minSpaceToFree = updateSize - unusedSpace
    putStrLn ("Used space: " ++ (show ((sizeMap fss) Map.! "/")) ++ "\nNeed to free at least " ++ (show minSpaceToFree) ++ " bytes")
    -- let soln = fold 
    -- print $ sizeMap fss
    -- 24390891 is too high

    -- I had forgotten about the 3000000 and was trying to find the smallest directory
    -- greater than the unused space...

    -- fixed it with some more thinking, logic, and `let`s

    putStrLn $ "Smallest directory size that frees enough space: " ++ (show $ minimum (Map.filter (>= minSpaceToFree) (sizeMap fss)))

-- Visualization
vis = do
    lines <- getLines "day7/input.txt"
    fss <- recurseAccumulate FSState { path = "/", sizeMap = Map.empty } lines
    let driveSize = 70000000
    let updateSize = 30000000
    let usedSpace = ((sizeMap fss) Map.! "/")
    let unusedSpace = driveSize - usedSpace
    let minSpaceToFree = updateSize - unusedSpace
    putStrLn ""
    putStrLn ("Used space: " ++ (show ((sizeMap fss) Map.! "/")) ++ "\nNeed to free at least " ++ (show minSpaceToFree) ++ " bytes")

    putStrLn $ "Smallest directory size that frees enough space: " ++ (show $ minimum (Map.filter (>= minSpaceToFree) (sizeMap fss)))