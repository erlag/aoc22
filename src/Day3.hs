{- Day 3: Rucksack Reorganization [https://adventofcode.com/2022/day/3]

1. Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?
2. Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of those item types?

A given rucksack always has the same number of items in each of its two compartments, 
so the first half of the characters represent items in the first compartment, 
while the second half of the characters represent items in the second compartment.

Lowercase item types a through z have priorities 1 through 26.
Uppercase item types A through Z have priorities 27 through 52.

Example:
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
=> 157 70
-}

module Day3 (run) where
import Util ( (▷), applyEach, intersection )
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map

type Item = Char
type Container = [Item] -- note: same as String

priorityScore :: Item -> Integer
priorityScore = (Map.!) priorityScores where
    priorityScores = Map.fromList $ zip priotityOrder [1..]
    priotityOrder = ['a'..'z']++['A'..'Z']

compartments :: Container -> [Container]
compartments items = [take n items, drop n items]
    where n = length items `div` 2

groups :: [Container] -> [[Container]]
groups = chunksOf 3

solve1 :: [Container] -> Integer
solve1 = map compartments ▷ concatMap intersection ▷ map priorityScore ▷ sum

solve2 :: [Container] -> Integer
solve2 = groups ▷ concatMap intersection ▷ map priorityScore ▷ sum

parse :: String -> [Container]
parse = words

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
