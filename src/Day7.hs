module Day7 where

import Utilities
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import Text.Regex.Base
import Text.Regex.PCRE
import Debug.Trace

-- I think creating the whole directory tree would be inefficient

data FSState = FSState { path :: String, sizeMap :: Map.Map String Int }

upOne :: FSState -> FSState
upOne fss = 
    let p = '/' : ((path fss) =~ "(([a-z]+)\\/)+" :: String) in
        if p == "/" then FSState { path = p, sizeMap = (sizeMap fss) }
        else FSState { path = init p, sizeMap = (sizeMap fss) }

parseCd :: FSState -> String -> FSState
parseCd fss cdCmd =
    if cdCmd == ".." then upOne fss -- remove last dir from path
    else if cdCmd == "/"  then FSState {path = "/", sizeMap = (sizeMap fss)}
    else
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
        let m = Map.Strict.insertWith (+) (path fss) size (sizeMap fss) in FSState {path = (path fss), sizeMap = m}
    else
        let 
            newPath = path (upOne fss)
            m = Map.Strict.insertWith (+) (path fss) size (sizeMap fss)
            in addSizeToMap (FSState {path = newPath, sizeMap = m}) size

debug = flip trace

-- expects command without "$ "
runCmd :: FSState -> String -> FSState
runCmd fss out =
    let cmd = take 4 out in
        if cmd == "$ cd" then
            let arg = drop 5 out in parseCd fss arg
        else if cmd == "$ ls" then
            -- do nothing, we'll handle that when we get the files
            fss
        else if cmd == "dir " then -- will be "dir "
            -- not a command so a dir
            -- we'll just match files since we don't care about directories
            fss
        else
            -- yay file!
            let match = out =~ "(\\d+)" :: String in
                FSState { path = (path fss), sizeMap = sizeMap (addSizeToMap fss (read match :: Int)) }

recurseAccumulate :: FSState -> [String] -> IO FSState
recurseAccumulate fss [] = 
    do return fss
recurseAccumulate fss list = 
    do
        -- putStrLn ((path fss) ++" " ++ (show $ sizeMap fss) ++ " " ++ (head list))
        (recurseAccumulate (runCmd fss (head list)) (drop 1 list))

part1 = do
    lines <- getLines "day7/input.txt"
    fss <- recurseAccumulate FSState { path = "/", sizeMap = Map.empty } lines
    -- let soln = fold 
    print $ sum $ Map.map (\x -> if x <= 100000 then x else 0) (sizeMap fss)

-- 24390891 is wrong

part2 = do
    lines <- getLines "day7/input.txt"
    fss <- recurseAccumulate FSState { path = "/", sizeMap = Map.empty } lines
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

    print $ minimum (Map.filter (>= minSpaceToFree) (sizeMap fss))