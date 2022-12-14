{- Day 14: Regolith Reservoir [https://adventofcode.com/2022/day/14]

Each input line describes a path that represents a shape of rock structure.
A path consists of line segments. Tiles covered by a line segment are occupied by rock.
Each x,y point indicates the end of a straight line connected the next point,
where x represents distance to the right and y represents distance down.

Units of sand that occupy one tile each fall one at atime from point 500,0.
Sand primarily falls down one step, secondarily down and to the left, thirdly down and to the right.
If all three alternatives are occupied the sand comes to rest.

1. How many units of sand come to rest before sand starts flowing into the abyss below?
2. There is a floor at two plus the highest y coordinate, not an abyss. How many units of sand come to rest until the source of the sand becomes blocked?

Example:
498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
=> 24 93
-}

module Day14 (run) where
import Util ( (▷), applyEach, asPair, both, zipPairWith, maybeFix, iterJust )
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, toList, insert, notMember)

type Input = [[Pos]]
type Pos = (Int, Int)
type Structure = Set Pos

move :: Pos -> (Int, Int) -> Pos
move = zipPairWith (+)

diff :: Pos -> Pos -> (Int, Int)
diff = zipPairWith (-)

towards :: Pos -> Pos -> Pos
a `towards` b = b `diff` a & both signum & move a

path :: [Pos] -> [Pos]
path [p] = [p]
path (a:b:t) | a == b    = path (b:t)
             | otherwise = a : path ((a `towards` b):b:t)

renderCave :: Input -> Structure
renderCave = concatMap path ▷ fromList

fallStep :: (Pos -> Bool) -> (Pos -> Bool) -> Pos -> Maybe Pos
fallStep available inBounds = applyEach [down, downLeft, downRight, stay] ▷ find available ▷ find inBounds
    where down = move (0, 1); downLeft = move (-1, 1); downRight = move (1, 1); stay = id

dropSand :: Pos -> (Pos -> Bool) -> (Pos -> Bool) -> Set Pos -> Maybe (Set Pos)
dropSand entryPoint isFloor isAbyss structure = entryPoint & maybeFix step <&> (`insert` structure)
    where available pos = notMember pos structure && (not.isFloor) pos
          inBounds = not.isAbyss
          step = fallStep available inBounds

fillSandAbyss :: Structure -> [Structure]
fillSandAbyss structure = structure & iterJust (dropSand entryPoint isFloor isAbyss)
    where abyssDepth = structure & toList & map snd & maximum & (+ 1)
          isAbyss = snd ▷ (>= abyssDepth)
          isFloor = const False

fillSandFloor :: Structure -> [Structure]
fillSandFloor structure = structure & iterJust (dropSand entryPoint isFloor isAbyss)
    where floorDepth = structure & toList & map snd & maximum & (+ 2)
          isFloor = snd ▷ (== floorDepth)
          isAbyss = const False

entryPoint :: Pos
entryPoint = (500, 0)

solve1 :: Input -> Int
solve1 = renderCave ▷ fillSandAbyss ▷ tail ▷ length

solve2 :: Input -> Int
solve2 = renderCave ▷ fillSandFloor ▷ tail ▷ length

parse :: String -> Input
parse = lines ▷ map (splitOn " -> " ▷ map (splitOn "," ▷ map read ▷ asPair))

run :: String -> [Int]
run = parse ▷ applyEach [solve1, solve2]
