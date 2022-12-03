import Utilities
import Data.Char
import Data.List
import Data.Set (toList, fromList)
data Rucksack = Rucksack String String deriving (Show) -- rucksack is an amazing word

-- https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

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

onlyTrue :: (String, [Bool]) -> [Char]
onlyTrue t =
    let indexedChars = zip [0..] (fst t) in
        map snd (filter (\(a, b) -> (snd t) !! a) indexedChars)

-- https://stackoverflow.com/questions/46967233/how-to-use-the-map-function-in-haskell
splitIntoThrees :: [String] -> [[String]]
splitIntoThrees strs = filter (\x -> length x == 3) (map (take 3) (tails strs))

getDuplicateChars :: ([String], [Char]) -> [Char]
getDuplicateChars ([], chars) = chars
getDuplicateChars (sacks, chars) =
    if null chars then
        getDuplicateChars (drop 2 sacks, onlyTrue ((sacks !! 0), (map (`elem` (sacks !! 1)) (sacks !! 0))))
    else
        getDuplicateChars (drop 1 sacks, onlyTrue (chars, (map (`elem` (sacks !! 0)) chars)))

iterOver :: ([String], [Char]) -> [Char]
iterOver ([], chars) = chars
iterOver (strs, chars) = iterOver ((drop 3 strs), (chars ++ mkUniq (getDuplicateChars ((take 3 strs), []))))

main = do
    lines <- getLines "input.txt"
    print (sum (map getPriority (iterOver (lines, []))))