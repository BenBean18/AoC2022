import Utilities
import Data.Char
import Data.List
data Rucksack = Rucksack String String deriving (Show) -- rucksack is an amazing word

-- https://stackoverflow.com/questions/28706843/getting-a-value-from-maybe-a-return-type-in-haskell
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Oops, you goofed up, fool."

getPriority :: Char -> Int
getPriority c = 
    if isLower c then (ord c - 97) + 1
    else if isUpper c then (ord c - 65) + 27
    else -1

parseRucksack :: String -> Rucksack
parseRucksack s = 
    let l = length s
        halves = splitAt (l `div` 2) s in
        Rucksack (fst halves) (snd halves)

getDuplicateChar :: Rucksack -> Char
getDuplicateChar (Rucksack first second) =
    first !! (fromJust (elemIndex True (map (`elem` second) first)))

getDupePriority :: String -> Int
getDupePriority s = getPriority (getDuplicateChar (parseRucksack s))

main = do
    lines <- getLines "input.txt"
    print (sum (map getDupePriority lines))