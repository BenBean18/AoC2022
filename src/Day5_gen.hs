import Data.Text.IO (readFile)
import System.Environment
import Data.Text (unpack)

replaceWithStr :: String -> String -> String
replaceWithStr contents str =   let a '!' = str !! 0
                                    a '@' = str !! 1
                                    a '#' = str !! 2
                                    a '$' = str !! 3
                                    a '%' = str !! 4
                                    a '^' = str !! 5
                                    a '&' = str !! 6
                                    a '*' = str !! 7
                                    a '(' = str !! 8
                                    a  c  = c in map a contents

main = do
    args <- getArgs
    contents <- Data.Text.IO.readFile "day5/input_with_msg_template.txt"
    putStrLn $ head args
    putStrLn $ unpack contents `replaceWithStr` (head args)