{- Day X: [https://adventofcode.com/2022/day/X]

1. 
2. 

Example:
=>
-}

module DayX (run) where
import Util

data Input

solve1 :: Input -> Integer
solve1 = undefined

solve2 :: Input -> Integer
solve2 = undefined

parse :: String -> Input
parse = undefined

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
