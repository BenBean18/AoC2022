module Day11_part2 where

-- FRICK
-- AoC2022: osCommitMemory: VirtualAlloc MEM_COMMIT failed to commit 11534336 bytes of memory  (error code: 1455): The paging file is too small for this operation to complete.

-- tried evaluating strictly, here's a new error:
{-
Round: 9914
GNU MP: Cannot allocate memory (size=4230479896)
-}

-- The only thing about the worry value that matters is if it's divisible by a small integer.
-- n * n 

-- I remembered talk of the Chinese remainder theorem last year
-- and thought it had something to do with modulo (remainder) so I checked
-- Google Trends and sure enough it spiked at midnight.

-- modulos have to be coprime for the theorem (GCD = 1) and that is
-- obviously true for prime numbers since their only divisors are
-- themselves and one, and the sample & my input have only prime moduluses

-- symmetrical imports :)
import Utilities
import Text.Regex.Base
import Text.Regex.PCRE
import Data.List.Split
import Data.List

import Debug.Trace
import Data.Maybe
import Text.Read

-- Declare custom datatypes
data Monkey = Monkey { items :: [Int], divisor :: Int, fn :: ([Monkey] -> [Monkey]), interactedCount :: Int }

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

multiplyMods :: [Monkey] -> Int
multiplyMods monkeys = (foldl (*) 1 $ map divisor monkeys)

-- math
crtSimplify :: Int -> Int -> Int
crtSimplify modsMultiplied worryLevel =
    worryLevel `mod` modsMultiplied

-- Parse operation like "new = old * 19" and simplifies using CRT instead of /3
-- only uses * and + (won't decrease your worry level...)
parseOperation :: String -> ([Monkey] -> Int -> Int)
parseOperation op monkeys a =
    let matches = op =~ "old ([*+]) (\\d+|(?:old))"
        modsMultiplied = multiplyMods monkeys in
        if length matches < 1 then error "Invalid monkey! Monkey business detected!"
        else
            let operator = head ((head matches) !! 1)
                operand  = readMaybe ((head matches) !! 2) :: Maybe Int in
                    if isNothing operand then -- it is "old"
                        if operator == '*' then crtSimplify modsMultiplied (a * a)
                        else if operator == '+' then crtSimplify modsMultiplied (a + a)
                        else error "Invalid monkey! Monkey business detected!"
                    else
                        if operator == '*' then crtSimplify modsMultiplied (a * (fromJust operand))
                        else if operator == '+' then crtSimplify modsMultiplied (a + (fromJust operand))
                        else error "Invalid monkey! Monkey business detected!"

getDivisor :: String -> Int
getDivisor s =
    read (last (head (s =~ "divisible by (\\d+)" :: [[String]]))) :: Int

parseTest :: String -> (Int -> Bool)
parseTest s i =
    let divisor = getDivisor s in i `mod` divisor == 0

debug = flip trace

-- Arguments: operation, test, monkey if true, monkey if false, monkey id
-- Returns a function [Monkey] -> [Monkey]
monkFunc :: ([Monkey] -> Int -> Int) -> (Int -> Bool) -> Int -> Int -> Int -> ([Monkey] -> [Monkey])
monkFunc operation test true false id monkeys =
    let currentItems = items (monkeys !! id) in
        if length currentItems == 0 then monkeys
        else
            let thisItem = operation monkeys (head currentItems)
                monkeyToThrowTo = if test thisItem then true else false
                destMonkeyItems = items (monkeys !! monkeyToThrowTo)
                newDestMonkeyItems = insert1D 0 thisItem destMonkeyItems
                newSelfItems = tail currentItems
                newMonkeys = (set1D id Monkey { items = newSelfItems, divisor = divisor (monkeys !! id), fn = fn (monkeys !! id), interactedCount = interactedCount (monkeys !! id) + 1 } (set1D monkeyToThrowTo Monkey { items = newDestMonkeyItems, divisor = divisor (monkeys !! monkeyToThrowTo), fn = fn (monkeys !! monkeyToThrowTo), interactedCount = interactedCount (monkeys !! monkeyToThrowTo) } monkeys)) in
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
            Monkey { items = startingItems, divisor = getDivisor monkey, fn = monkFunc operation test true false id, interactedCount = 0 }

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

part2' lines = do
    let monkeys = parseMonkeys lines
    let monkeysAfter10000 = monkeys `monkeyRounds` 10000
    print monkeysAfter10000
    let sorted = sortDesc monkeysAfter10000
    let monkeyBusiness = interactedCount (head sorted) * interactedCount (sorted !! 1)
    putStrLn $ "Amount of monkey business: " ++ (show monkeyBusiness)