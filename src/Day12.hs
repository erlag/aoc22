{- Day 12: Hill Climbing Algorithm [https://adventofcode.com/2022/day/12]

1. What is the fewest steps required to move from S to E?
2. What is the fewest steps required to move to E from any square with elevation a?

During each step, you can move exactly one square up, down, left, or right. 
The elevation of the destination square can be at most one higher than the elevation of your current square.
a is the lowest elevation, b is the next-lowest, and so on up to the highest elevation, z.
S has elevation a and E has elevation z.

Example:
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
=> 31 29
-}

module Day12 (run) where
import Util ( (▷), applyEach, zip2D )
import Algorithm.Search (bfs)
import Data.Function ((&))
import Data.List (lookup)
import Data.Tuple (swap)
import Data.Maybe (fromJust, mapMaybe, fromMaybe)
import Data.Char (ord)
import qualified Data.Map as M

data Input = Input {
    start :: Coordinate,
    end :: Coordinate,
    terrain :: ElevationMap
}
type Coordinate = (Int, Int)
type ElevationMap = M.Map Coordinate Elevation
type Elevation = Int

passable :: Elevation -> Elevation -> Bool
passable a b = b <= a + 1

passableEdge :: ElevationMap -> Coordinate -> Coordinate -> Bool
passableEdge terrain from to = (passable <$> (terrain M.!? from) <*> (terrain M.!? to)) & fromMaybe False

directions :: Coordinate -> [Coordinate]
directions (row, col) = [(row, col - 1), (row, col + 1), (row + 1, col), (row - 1, col)]

neighbors :: (Coordinate -> Coordinate -> Bool) -> Coordinate -> [Coordinate]
neighbors test origin = directions origin & filter (test origin)


solve1 :: Input -> Int
solve1 Input { start, end, terrain } = bfs (neighbors (passableEdge terrain)) (== end) start & fromJust & length

solve2 :: Input -> Int
solve2 Input { start, end, terrain } = bfs (neighbors (flip (passableEdge terrain))) at0 end & fromJust & length
    where at0 = (terrain M.!) ▷ (== 0)


gridCoordinates :: [[Coordinate]]
gridCoordinates = [[(row, col) | col <- [0..]] | row <- [0..]]

coordinatesAssoc :: [[a]] -> [(Coordinate, a)]
coordinatesAssoc = zip2D gridCoordinates ▷ concat

readElevation :: Char -> Int
readElevation 'S' = readElevation 'a'
readElevation 'E' = readElevation 'z'
readElevation c = ord c - ord 'a'

parse :: String -> Input
parse = lines ▷ coordinatesAssoc ▷ (\assoc -> Input {
    start = assoc & map swap & lookup 'S' & fromJust,
    end = assoc & map swap & lookup 'E' & fromJust,
    terrain = assoc & M.fromList & M.map readElevation
})

run :: String -> [Int]
run = parse ▷ applyEach [solve1, solve2]
