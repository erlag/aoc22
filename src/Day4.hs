{- Day 4: Camp Cleanup [https://adventofcode.com/2022/day/4]

1. In how many assignment pairs does one range fully contain the other?
2. In how many assignment pairs do the ranges overlap?

Example:
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
=> 2 4
-}

module Day4 (run) where
import Util ( (▷), applyEach, count, asPair, mapPair )
import Data.List.Split (splitOn)

type Input = [(Range, Range)]
data Range = ClosedRange { start :: Integer, end :: Integer } deriving (Show,Eq)

contains :: Range -> Range -> Bool
contains a b = start a >= start b && end a <= end b

eitherOneContainsTheOther :: (Range, Range) -> Bool
eitherOneContainsTheOther (a, b) = contains a b || contains b a

overlaps :: (Range, Range) -> Bool
overlaps (a, b) = end a >= start b && end b >= start a

solve1 :: Input -> Integer
solve1 = count eitherOneContainsTheOther

solve2 :: Input -> Integer
solve2 = count overlaps

parse :: String -> Input
parse = lines ▷ map readPairOfRanges where
    readPairOfRanges = splitOn "," ▷ map readRange ▷ asPair
    readRange = splitOn "-" ▷ map read ▷ asPair ▷ asRange
    asRange (start, end) = ClosedRange { start, end }

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
