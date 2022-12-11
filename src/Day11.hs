module Day11 where

-- symmetrical imports :)
import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List.Split
import Data.List

import Debug.Trace
import Data.Maybe
import Text.Read
import qualified Day11_part2

-- Declare custom datatypes
data Monkey = Monkey { items :: [Int], fn :: ([Monkey] -> [Monkey]), interactedCount :: Int }

instance Ord Monkey where
    m1 `compare` m2 = (interactedCount m1) `compare` (interactedCount m2)
instance Eq Monkey where
    m1 == m2 = (items m1) == (items m2) && interactedCount m1 == interactedCount m2
instance Show Monkey where
    show m = show $ items m

insert1D :: Int -> a -> [a] -> [a]
insert1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ zs

set1D :: Int -> a -> [a] -> [a]
set1D i v l = let (ys,zs) = splitAt i l in ys ++ [v] ++ (tail zs)

{-
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
-}

-- Parse operation like "new = old * 19" and divides by 3
-- only uses * and + (won't decrease your worry level...)
parseOperation :: String -> (Int -> Int)
parseOperation op a =
    let matches = op =~ "old ([*+]) (\\d+|(?:old))" in
        if length matches < 1 then error "Invalid monkey! Monkey business detected!"
        else
            let operator = head ((head matches) !! 1)
                operand  = readMaybe ((head matches) !! 2) :: Maybe Int in
                    if isNothing operand then -- it is "old"
                        if operator == '*' then (a * a) `div` 3
                        else if operator == '+' then (a + a) `div` 3
                        else error "Invalid monkey! Monkey business detected!"
                    else
                        if operator == '*' then (a * (fromJust operand)) `div` 3
                        else if operator == '+' then (a + (fromJust operand)) `div` 3
                        else error "Invalid monkey! Monkey business detected!"

parseTest :: String -> (Int -> Bool)
parseTest s i =
    let divisor = read (last (head (s =~ "divisible by (\\d+)" :: [[String]]))) :: Int in i `mod` divisor == 0

debug = flip trace

-- Arguments: operation, test, monkey if true, monkey if false, monkey id
-- Returns a function [Monkey] -> [Monkey]
monkFunc :: (Int -> Int) -> (Int -> Bool) -> Int -> Int -> Int -> ([Monkey] -> [Monkey])
monkFunc operation test true false id monkeys =
    let currentItems = items (monkeys !! id) in
        if length currentItems == 0 then monkeys
        else
            let thisItem = operation $ head currentItems
                monkeyToThrowTo = if test thisItem then true else false
                destMonkeyItems = items (monkeys !! monkeyToThrowTo)
                newDestMonkeyItems = insert1D 0 thisItem destMonkeyItems
                newSelfItems = tail currentItems
                newMonkeys = (set1D id Monkey { items = newSelfItems, fn = fn (monkeys !! id), interactedCount = interactedCount (monkeys !! id) + 1 } (set1D monkeyToThrowTo Monkey { items = newDestMonkeyItems, fn = fn (monkeys !! monkeyToThrowTo), interactedCount = interactedCount (monkeys !! monkeyToThrowTo) } monkeys)) in
                    monkFunc operation test true false id newMonkeys

-- parseMonkey takes a whole monkey:
{-
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3
-}
parseMonkey :: String -> Monkey
parseMonkey monkey =
    let id = read (last (head (monkey =~ "Monkey (\\d+):" :: [[String]]))) :: Int
        startingItemsRaw = monkey =~ "Starting items: .+" :: String
        startingItems = map (\x -> read (last x) :: Int) (startingItemsRaw =~ "(\\d+)(?:[, ]*)" :: [[String]])
        operationRaw = monkey =~ "Operation: .+" :: String
        operation = parseOperation operationRaw
        testRaw = monkey =~ "Test: .+" :: String
        test = parseTest monkey
        true = read (last (head (monkey =~ "If true: throw to monkey (\\d+)" :: [[String]]))) :: Int
        false = read (last (head (monkey =~ "If false: throw to monkey (\\d+)" :: [[String]]))) :: Int in
            Monkey { items = startingItems, fn = monkFunc operation test true false id, interactedCount = 0 }

parseMonkeys :: [String] -> [Monkey]
parseMonkeys strs = map (parseMonkey . concat . (\y -> map (\x -> x ++ "\n") y)) (divvy 6 7 strs)

monkeyRound' :: [Monkey] -> Int -> [Monkey]
monkeyRound' monkeys index =
    if index == length monkeys then monkeys
    else monkeyRound' (fn (monkeys !! index) monkeys) (index+1)

monkeyRound monkeys = monkeyRound' monkeys 0

monkeyRounds :: [Monkey] -> Int -> [Monkey]
monkeyRounds monkeys 0 = monkeys
monkeyRounds monkeys numRounds =
    monkeyRounds (monkeyRound monkeys) (numRounds-1)

-- from https://ro-che.info/articles/2016-04-02-descending-sort-haskell
sortDesc = sortBy (flip compare)

part1 = do
    lines <- getLines "day11/input.txt"
    let monkeys = parseMonkeys lines
    let monkeysAfter20 = monkeys `monkeyRounds` 20
    let sorted = sortDesc monkeysAfter20
    let monkeyBusiness = interactedCount (head sorted) * interactedCount (sorted !! 1)
    putStrLn $ "Amount of monkey business: " ++ (show monkeyBusiness)