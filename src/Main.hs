module Main where

import System.Environment
import qualified Day4

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then
        let part = (read (args !! 1) :: Int)
            day = (read (args !! 0) :: Int) in
                if part == 1 then
                    if day == 4 then Day4.part1
                    else print "I haven't solved that yet (or it doesn't exist)"
                else if part == 2 then
                    if day == 4 then Day4.part2
                    else print "I haven't solved that yet (or it doesn't exist)"
                else print "Usage: AoC2022 <day> <part>"
    else print "Usage: AoC2022 <day> <part>"