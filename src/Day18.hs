{- Day 18: Boiling Boulders [https://adventofcode.com/2022/day/18]

To approximate the surface area, count the number of sides of each cube that are not immediately connected to another cube.

1. What is the surface area of your scanned lava droplet?
2. What is the exterior surface area of your scanned lava droplet?

Example:
2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
=> 64 58
-}
module Day18 (run) where
import Util ( (▷), applyBoth, applyEach, add3, zip3With, asTriplet )
import Data.Function ( (&) )
import Data.Bifunctor ( bimap )
import Data.List.Split ( splitOn )
import qualified Data.Set as S ( Set, empty, null, size, member, difference, union, take, findMin, findMax, fold, toList, fromList )

type Point3 = (Int, Int, Int)
type Shape = S.Set Point3

neighbors :: Point3 -> [Point3]
neighbors = applyEach (add3 <$> [(1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1)])

neighborsWithin :: Shape -> Point3 -> [Point3]
neighborsWithin shape = neighbors ▷ filter (`S.member` shape)

countHiddenFaces :: Shape -> Int
countHiddenFaces shape = shape & S.toList & concatMap (neighborsWithin shape) & length

solve1 :: Shape -> Int
solve1 input = totalFaces - countHiddenFaces input where totalFaces = 6 * S.size input


boundingBoxCorners :: Shape -> (Point3, Point3)
boundingBoxCorners shape = shape & applyBoth (S.fold min3 firstElem, S.fold max3 firstElem)
    where min3 = zip3With min; max3 = zip3With max; firstElem = S.findMin shape

fillBox :: Point3 -> Point3 -> Shape
fillBox (minX, minY, minZ) (maxX, maxY, maxZ) =
    [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]] & S.fromList

fillExtendedBoundingBox :: Shape -> Shape
fillExtendedBoundingBox shape = fillBox (minBound `add3` (-1,-1,-1)) (maxBound `add3` (1,1,1))
    where (minBound, maxBound) = boundingBoxCorners shape


flood :: (Point3 -> [Point3]) -> Shape -> Shape
flood connections = flood' S.empty where
    flood' closed open | S.null open = closed
                       | otherwise = flood' (closed `S.union` open) (expand open `S.difference` closed)
    expand = S.toList ▷ concatMap connections ▷ S.fromList

findCavities :: Shape -> Shape
findCavities shape =
    let invertedShape = fillExtendedBoundingBox shape `S.difference` shape
        outside = flood (neighborsWithin invertedShape) (S.take 1 invertedShape)
    in invertedShape `S.difference` outside

fillCavities :: Shape -> Shape
fillCavities input = input `S.union` findCavities input

solve2 :: Shape -> Int
solve2 = fillCavities ▷ solve1


parse :: String -> Shape
parse = lines ▷ map (splitOn "," ▷ map read ▷ asTriplet) ▷ S.fromList

run :: String -> [Int]
run = parse ▷ applyEach [solve1, solve2]
