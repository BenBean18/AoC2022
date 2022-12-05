module Main where

import System.Environment
import qualified Day4
import qualified Day5

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then
        let part = (read (args !! 1) :: Int)
            day = (read (args !! 0) :: Int) in
                if part == 1 then
                    if day == 4 then Day4.part1
                    else if day == 5 then Day5.part1
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == 2 then
                    if day == 4 then Day4.part2
                    else if day == 5 then Day5.part2
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else putStrLn "Usage: AoC2022 <day> <part>"
    else putStrLn "Usage: AoC2022 <day> <part>"