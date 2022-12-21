{- Day 20: Grove Positioning System: [https://adventofcode.com/2022/day/20]

The encrypted file is a list of numbers.
To mix the file, move each number forward or backward in the file a number of positions equal to the value of the number being moved, wrapping around the list as necessary.
The numbers should be moved in the order they originally appear in the encrypted file.

1. Mix your encrypted file exactly once. What is the sum of the three numbers that form the grove coordinates?
2. Multiply each number by the decryption key 811589153 before you begin. Mix your encrypted file ten times. What is the sum of the three numbers that form the grove coordinates?

The grove coordinates can be found by adding together the 1000th, 2000th, and 3000th numbers after the value 0, wrapping around the list as necessary. 

Example:
1
2
-3
3
-2
0
4
=> 3 1623178306
-}

module Day20 (run) where
import Util ( (▷), applyEach )
import Data.Function ( (&) )
import Data.Foldable ( toList )
import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import Data.Sequence ( Seq, Seq(..), fromList, insertAt )

type Input = [Int]

mix' :: Int -> Int -> Int -> Seq (Int, Int) -> Seq (Int, Int)
mix' iters len step seq@((i, x) :<| rest) | iters == 0  = seq                                    -- no iterations left. finished mixing
                                          | step == len = mix' (iters - 1) len 0 seq             -- processed all elements. start next iteration.
                                          | i /= step   = mix' iters len step (rest :|> (i, x))  -- wrong element for this step, skip to the next.
                                          | otherwise   = mix' iters len (step+1) (insertAt (x `mod` (len - 1)) (i, x) rest) -- shift element within sequence according to its value.

mix :: Int -> [Int] -> [Int]
mix times xs = xs & zip [0..] & fromList & mix' times (length xs) 0 & toList & map snd

answer :: [Int] -> Int
answer xs = let baseIdx = elemIndex 0 xs & fromJust
                at idx = xs !! ((baseIdx + idx) `mod` length xs)
            in at 1000 + at 2000 + at 3000

solve1 :: Input -> Int
solve1 = mix 1 ▷ answer

solve2 :: Input -> Int
solve2 = map (*811589153) ▷ mix 10 ▷ answer

parse :: String -> Input
parse = lines ▷ map read

run :: String -> [Int]
run = parse ▷ applyEach [solve1, solve2]
