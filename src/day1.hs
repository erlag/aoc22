{- Day 1: Calorie Counting [https://adventofcode.com/2022/day/1]

1. Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
2. Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total?

Example:
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
=> 24000 45000
-}

module Day1 (run) where
import Util ( (▷), applyEach, blocks, readWords, sortDesc )

parse :: String -> [[Integer]]
parse = blocks ▷ map readWords

prepare :: [[Integer]] -> [Integer]
prepare = map sum

solve1 :: [Integer] -> Integer
solve1 = maximum

solve2 :: [Integer] -> Integer
solve2 = sortDesc ▷ take 3 ▷ sum

run :: String -> [Integer]
run = parse ▷ prepare ▷ applyEach [solve1, solve2]
