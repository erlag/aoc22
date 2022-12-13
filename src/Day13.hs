{- Day 13: Distress Signal [https://adventofcode.com/2022/day/13]

Lists are compared element-wise in natural order. But when comparing a list against a value, the value is treated as a singleton list.

1. Determine which pairs of packets are already in the right order. What is the sum of the indices of those pairs?
2. Include two additional divider packets [[2]] and [[6]]. Sort all packets (ignoring blank lines). Determine the indices of the two divider packets and multiply them together.

Example:
[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
=> 13
-}

module Day13 (run) where
import Util
import Data.List (sort)
import Text.Megaparsec ( sepBy, (<|>), between )
import Text.Megaparsec.Char ( newline, string )
import Text.Megaparsec.Char.Lexer (decimal)

type Input = [[Tree Int]]
data Tree a = Branch [Tree a] | Leaf a deriving (Eq)

instance Ord a => Ord (Tree a) where
  (Leaf a) `compare` (Leaf b) = a `compare` b
  (Leaf a) `compare` (Branch bs) = [Leaf a]  `compare` bs
  (Branch as) `compare` (Leaf b) = as  `compare` [Leaf b]
  (Branch as) `compare` (Branch bs) = as  `compare` bs

isSortedPair :: Ord a => (a, a) -> Bool
isSortedPair (a, b) = a <= b

findIndices :: (a -> Bool) -> [a] -> [Integer]
findIndices p = zip [1..] ▷ filter (snd ▷ p) ▷ map fst

solve1 :: Input -> Integer
solve1 = map asPair ▷ findIndices isSortedPair ▷ sum

distressSignals :: [Tree Int]
distressSignals = map (applyParser treeParser) ["[[2]]", "[[6]]"]

solve2 :: Input -> Integer
solve2 = concat ▷ (++ distressSignals) ▷ sort ▷ findIndices (`elem` distressSignals) ▷ product

parse :: String -> Input
parse = blocks ▷ map (lines ▷ map (applyParser treeParser))

treeParser :: Parser (Tree Int)
treeParser = Leaf <$> decimal <|> between (string "[") (string "]") (Branch <$> treeParser `sepBy` string ",")

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
