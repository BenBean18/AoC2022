module Day13 where

import Utilities
import GHC.List (uncons)
import Data.Maybe
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List.Split
import Debug.Trace
import Data.List (sortBy, elemIndex)
import Criterion.Main
import System.Environment

-- https://stackoverflow.com/questions/48492142/replace-a-substring-from-a-string-in-haskell
import Data.Text(pack, unpack, replace)

replaceStr :: String -> String -> String -> String
replaceStr s1 s2 str = unpack . replace (pack s1) (pack s2) $ (pack str)

-- https://stackoverflow.com/questions/2973284/type-conditional-controls-in-haskell
data List a = Elem a | Nested [List a]
  deriving (Show, Eq)

compareList :: (Ord a, Show a) => List a -> List a -> Ordering
compareList (Elem e1) (Elem e2) = compare e1 e2
compareList (Nested []) (Nested []) = EQ
compareList (Nested l1) (Nested l2) =
    let u1_ = uncons l1
        u2_ = uncons l2 in
            if isNothing u2_ then GT else if isNothing u1_ then LT else
                let u1 = fromJust u1_
                    u2 = fromJust u2_
                    comp = compareList (fst u1) (fst u2) in {-(trace $ (show u1) ++ "\n" ++ (show u2) ++ "\n")-} (
                if comp == EQ then compareList (Nested (snd u1)) (Nested (snd u2))
                else comp)
compareList (Nested l1) (Elem e1) = compareList (Nested l1) (Nested [Elem e1])
compareList (Elem e1) (Nested l1) = compareList (Nested [Elem e1]) (Nested l1)

-- [[1],[2,3,4]]
-- [ means Nested parseList tail
-- [ means Nested parseList tail
-- 1 means Elem 1
-- ] means return
-- [ means Nested parseList tail
-- 2 means Elem 2
-- , means continue

-- splitOn "," "[[1],[2,3,4]]"
-- ["[[1]", "[2", "3", "4]]"] 

-- splitOn "[" "[[1],[2,3,4]]"
-- ["", "", "1]," ,"2,3,4]]"]
-- so:
-- map parseList on the split list
-- "" = new nested
-- 


-- TODO:
-- [ -> Nested [
-- ] unchanged
-- (\d+) -> Elem $1
-- Add a l1 or l2 before it
-- & evaluate it

-- (map ((\x -> read x :: Int) . head) ("2,3,4]]" =~ "(\\d+)" :: [[String]]))
-- ["", "", "1]," ,"2,3,4]]"]
-- parseListItem :: [String] -> List Int
-- parseListItem strs = Nested (map (Elem . (\x -> read x :: Int) . head) (str =~ "(\\d+)" :: [[String]]))
-- -- do something with strs
-- [ = currentList : Nested (parseListItem tail (Nested []))
-- ] = currentList
-- , = currentList : (parseListItem tail (Nested []))
-- anything else = read into an int

-- parseList :: [String] -> List Int -> List Int
-- parseList ("[":strs) (Nested currentList) = Nested (currentList ++ [(parseList strs (Nested []))])
-- parseList ("]":strs) currentList = currentList -- need to indicate that there's more to come after this (or not)
-- parseList (",":strs) currentList = parseList strs currentList
-- parseList ("":strs) currentList = parseList strs currentList
-- parseList (str:strs) (Nested currentList) = parseList strs (Nested (currentList ++ [Elem (read str :: Int)]))

-- 5985 too low

parseAndCombineUntilEmpty :: [String] -> List Int -> ([String], List Int)
parseAndCombineUntilEmpty [] list = ([], list)
parseAndCombineUntilEmpty (str:strs) list =
    let (parsedStrs, (Nested parsedList)) = parseList strs (Nested [])
        (nextStrs, (Nested nextList)) = parseAndCombineUntilEmpty parsedStrs list in
        (nextStrs, Nested (parsedList ++ nextList))

parseList :: [String] -> List Int -> ([String], List Int)
parseList ("[":strs) (Nested currentList) = 
    let (parsedStrs, (Nested parsedList)) = parseList strs (Nested [])
        (nextStrs, (Nested nextList)) = parseList parsedStrs (Nested parsedList) in
        (nextStrs, Nested (currentList ++ [(Nested nextList)]))
-- parseList ("[":"":"]":strs) currentList = parseList ("[":"":"]":"":"]":strs) currentList
parseList ("]":strs) currentList = (strs, currentList) -- need to indicate that there's more to come after this (or not)
parseList (",":strs) currentList = parseList strs currentList
parseList ("":strs) currentList = parseList strs currentList
parseList (str:strs) (Nested currentList) = parseList strs (Nested (currentList ++ [Elem (read str :: Int)]))
parseList [] currentList = ([], currentList)

parseString :: String -> List Int
parseString s = snd (parseList (split (oneOf "[],") (replaceStr "]" "]]" s)) (Nested []))

isCorrectOrder :: [String] -> Bool
isCorrectOrder [a, b] = (compareList (parseString a) (parseString b)) /= GT

part1' lines = do
    let pairs = divvy 2 3 lines
    let pairsCorrect = map isCorrectOrder pairs
    let sumOfIndices = foldl (+) 0 [if (fst t) then (snd t) else 0 | t <- (zip pairsCorrect [1..(length pairsCorrect)])]
    print sumOfIndices

-- Part 2

-- Add [[2]] and [[6]]
-- Sort into correct order
-- div1 = Nested [Nested [Nested [Elem 2]]]
-- div2 = Nested [Nested [Nested [Elem 6]]]
part2' lines = do
    let nonBlankLines = (filter (/= "") lines)
    let packets_ = map parseString nonBlankLines
    let div1 = (Nested [Nested [Nested [Elem 2]]])
    let div2 = (Nested [Nested [Nested [Elem 6]]])
    let packets = div1:div2:packets_
    let sorted = sortBy compareList packets
    let div1Location = (fromMaybe (-1) $ elemIndex div1 sorted) + 1
    let div2Location = (fromMaybe (-1) $ elemIndex div2 sorted) + 1
    print $ div1Location * div2Location

-- Benchmarking
part1 = do
    lines <- getLines "day13/input.txt"
    part1' lines

part2 = do
    lines <- getLines "day13/input.txt"
    part2' lines

time lines =
    withArgs ["--output", "day13.html"] $ defaultMain [
        bench "part1" $ nfIO $ part1' lines
      , bench "part2" $ nfIO $ part2' lines
    ]

benchmark = do
    lines <- getLines "day13/input.txt"
    time lines