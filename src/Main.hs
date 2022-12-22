module Main where

import System.Environment
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 then
        let part = args !! 1
            day = (read (args !! 0) :: Int) in
                if part == "1" then
                    if day == 4 then Day4.part1
                    else if day == 5 then Day5.part1
                    else if day == 6 then Day6.part1
                    else if day == 7 then Day7.part1
                    else if day == 8 then Day8.part1
                    else if day == 9 then Day9.part1
                    else if day == 10 then Day10.part1
                    else if day == 11 then Day11.part1
                    else if day == 12 then Day12.part1
                    else if day == 13 then Day13.part1
                    else if day == 14 then Day14.part1
                    else if day == 15 then Day15.part1
                    else if day == 16 then Day16.part1
                    else if day == 17 then Day17.part1
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "2" then
                    if day == 4 then Day4.part2
                    else if day == 5 then Day5.part2
                    else if day == 6 then Day6.part2
                    else if day == 7 then Day7.part2
                    else if day == 8 then Day8.part2
                    else if day == 9 then Day9.part2
                    else if day == 10 then Day10.part2
                    else if day == 11 then Day11.part2
                    else if day == 12 then Day12.part2
                    else if day == 13 then Day13.part2
                    else if day == 14 then Day14.part2
                    else if day == 15 then Day15.part2
                    else if day == 16 then Day16.part2
                    else if day == 17 then Day17.part2
                    else putStrLn "I haven't solved that yet (or it doesn't exist)"
                else if part == "v" then
                    if day == 7 then Day7.vis
                    else putStrLn "I haven't visualized that day yet (or it doesn't exist)"
                else if part == "b" then
                    if day == 4 then Day4.benchmark
                    else if day == 5 then Day5.benchmark
                    else if day == 6 then Day6.benchmark
                    else if day == 7 then Day7.benchmark
                    else if day == 8 then Day8.benchmark
                    else if day == 9 then Day9.benchmark
                    else if day == 10 then Day10.benchmark
                    else if day == 11 then Day11.benchmark
                    else if day == 12 then Day12.benchmark
                    else if day == 13 then Day13.benchmark
                    else if day == 14 then Day14.benchmark
                    else if day == 15 then Day15.benchmark
                    else if day == 16 then Day16.benchmark
                    else putStrLn "I haven't benchmarked that day yet (or it doesn't exist)"
                else putStrLn "Usage: AoC2022 <day> <part || \"v\" for visualization>"
    else putStrLn "Usage: AoC2022 <day> <part || \"v\" for visualization>"