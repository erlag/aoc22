{- Day 6: Tuning Trouble |https://adventofcode.com/2022/day/6]

How many characters need to be processed before the first start-of-packet / start-of-message marker is detected?

1. The start of a packet is indicated by a sequence of four characters that are all different.
2. A start-of-message marker is just like a start-of-packet marker, except it consists of 14 distinct characters rather than 4.

Example:
mjqjpqmgbljsphdztnvjfqwrcgsmlb
=> 7 19
-}

module Day6 (run) where
import Util ( (▷), applyEach )
import Data.List (findIndex, tails)
import Data.Maybe ( fromJust ) 

allDistinct :: Eq a => [a] -> Bool
allDistinct [] = True
allDistinct (h:t) = h `notElem` t && allDistinct t

initialIndexWhere :: ([a] -> Bool) -> [a] -> Int
initialIndexWhere p = tails ▷ findIndex p ▷ fromJust

solve1 :: String -> Int
solve1 = initialIndexWhere (allDistinct . take 4) ▷ (+4)

solve2 :: String -> Int
solve2 = initialIndexWhere (allDistinct . take 14) ▷ (+14)

run :: String -> [Int]
run = applyEach [solve1, solve2]
