{- Day 9: Rope Bridge [https://adventofcode.com/2022/day/9]

The head and its tail must always be touching (diagonally adjacent and even overlapping both count as touching).
If the head is two steps directly up, down, left, or right from the tail, the tail must also move one step in that direction.
Otherwise, if the head and tail aren't touching and aren't in the same row or column, the tail moves one step diagonally to keep up.
Simulate your complete hypothetical series of motions. How many positions does the tail of the rope visit at least once?

1. Consider a rope with a knot at each end; these knots mark the head and the tail of the rope.
2. Consider a larger rope with ten knots.

Example:
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
=> 13 1

Larger example:
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
=> _ 36
-}

module Day9 (run) where
import Util ( (▷), applyEach, both, asPair, mapPair, add2, sub2, pipeline, countUnique )
import Data.Bifunctor ( bimap )
import Data.Function ( (&) )

type Input = [Instruction]
type Instruction = (Direction, Int)
data Direction = R | L | U | D  deriving (Read, Show)
type Pos = (Int, Int)

delta :: Direction -> (Int, Int)
delta = \case { R -> (1, 0); L -> (-1, 0) ; U -> (0, 1) ; D -> (0, -1) }

move :: Pos -> Direction -> Pos
move pos = delta ▷ add2 pos

distance :: Pos -> Pos -> Int
distance a b = sub2 a b & both abs & uncurry max

chase :: Pos -> Pos -> Pos
t `chase` h | distance h t <= 1 = t
            | otherwise = sub2 h t & both signum & add2 t

traceRopePaths :: [Direction] -> [[Pos]]
traceRopePaths = scanl move origin ▷ iterate (scanl chase origin)
    where origin = (0, 0)

solve1 :: [Direction] -> Integer
solve1 = traceRopePaths ▷ (!!1) ▷ countUnique

solve2 :: [Direction] -> Integer
solve2 = traceRopePaths ▷ (!!9) ▷ countUnique

simplify :: Input -> [Direction]
simplify = concatMap $ \(direction, steps) -> replicate steps direction

parse :: String -> Input
parse = lines ▷ map (words ▷ asPair ▷ mapPair (read, read))

run :: String -> [Integer]
run = parse ▷ simplify ▷ applyEach [solve1, solve2]
