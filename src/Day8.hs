{- Day 8: Treetop Tree House [https://adventofcode.com/2022/day/8]

1. Consider your map; how many trees are visible from outside the grid?
2. Consider each tree on your map. What is the highest scenic score possible for any tree? A tree's scenic score is found by multiplying together its viewing distance in each of the four directions.

Example:
30373
25512
65332
33549
35390
=>
21 8
-}

module Day8 (run) where
import Util ( (▷), applyEach, readChars, count )
import Data.Function ((&))
import Data.List (transpose, tails)

type Height = Integer
type Input = [[Height]]

rotations :: [[[a]] -> [[a]]]
rotations = [id, transpose ▷ map reverse, reverse ▷ map reverse, transpose ▷ reverse]

reverseRotations :: [[[a]] -> [[a]]]
reverseRotations = [id, transpose . map reverse, reverse . map reverse, transpose . reverse]

forEachRotation :: ([[a]] -> [[b]]) -> [[a]] -> [[[b]]]
forEachRotation f = applyEach rotations ▷ map f ▷ zipWith ($) reverseRotations

reduceTo2d :: (a -> a -> a) -> [[[a]]] -> [[a]]
reduceTo2d f = foldr1 (zipWith (zipWith f))


treesVisibleInLine :: Height -> [Height] -> [Bool]
treesVisibleInLine threshold [] = []
treesVisibleInLine threshold (h:t) | h <= threshold = False : treesVisibleInLine threshold t
treesVisibleInLine threshold (h:t) | h > threshold = True : treesVisibleInLine h t

treesVisibleFromSides :: [[Height]] -> [[Bool]]
treesVisibleFromSides = forEachRotation (map (treesVisibleInLine (-1))) ▷ reduceTo2d (||)

solve1 :: Input -> Integer
solve1 = treesVisibleFromSides ▷ concat ▷ count id


viewingDistance :: Height -> [Height] -> Integer
viewingDistance threshold [] = 0
viewingDistance threshold (h:t) | h >= threshold = 1
viewingDistance threshold (h:t) | h < threshold = 1 + viewingDistance threshold t

forwardViewingDistances :: [[Height]] -> [[Integer]]
forwardViewingDistances = map (tails ▷ init ▷ map (\(h:t) -> viewingDistance h t))

treeScenicScores :: [[Height]] -> [[Integer]]
treeScenicScores = forEachRotation forwardViewingDistances ▷ reduceTo2d (*)

solve2 :: Input -> Integer
solve2 = treeScenicScores ▷ concat ▷ maximum


parse :: String -> Input
parse = lines ▷ map readChars

run :: String -> [Integer]
run = parse  ▷ applyEach [solve1, solve2]
