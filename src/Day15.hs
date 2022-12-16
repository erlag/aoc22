{- Day 15: Beacon Exclusion Zone [https://adventofcode.com/2022/day/15]

Each sensor knows its own position and the position of the closest beacon as measured by the Manhattan distance. 
There is never a tie where two beacons are the same distance to a sensor.

None of the detected beacons seem to be producing the distress signal, 
so you'll need to work out where the distress beacon is by working out where it isn't. 

1. In the row where y=2000000, how many positions cannot contain a beacon?
2. Find the only possible position for the distress beacon. What is its tuning frequency?

The tuning frequency for a point is 4000000 * x + y

Example (for rowOfInterest = 10 and search area 0..20,0..20):
Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
=> 26 56000011
-}

module Day15 (run) where
import Util ( applyEach, applyParser, countUnique, (▷), Parser )
import Data.Maybe (fromJust, listToMaybe)
import Data.Function ((&))
import qualified Data.RangeSet.List as Rs (RSet, fromList, toList, empty, union, size, singletonRange, difference)
import Text.Megaparsec ( sepBy )
import Text.Megaparsec.Char ( newline, string )
import Text.Megaparsec.Char.Lexer (signed, decimal)

type Point = (Int, Int)
data Reading = Reading { sensor :: Point, closestBeacon :: Point }  deriving (Show)
type Input = [Reading]

dist1d :: Int -> Int -> Int
dist1d a b = abs (a - b)

dist2d :: Point -> Point -> Int
dist2d (x1, y1) (x2, y2) = dist1d x1 x2 + dist1d y1 y2

readingCoverageInRow :: Int -> Reading -> Rs.RSet Int
readingCoverageInRow row Reading { sensor=(x,y), closestBeacon } =
    let radius = dist2d (x,y) closestBeacon
        radiusInRow = radius - dist1d y row
    in Rs.singletonRange (x - radiusInRow, x + radiusInRow)

rowCoverage :: Int -> [Reading] -> Rs.RSet Int
rowCoverage row = map (readingCoverageInRow row) ▷ foldr Rs.union Rs.empty

rowBlanks :: Int -> Rs.RSet Int -> [Reading] -> Rs.RSet Int
rowBlanks row searchRange = rowCoverage row ▷ Rs.difference searchRange

findBlank :: Rs.RSet Int -> [Reading] -> Maybe Point
findBlank searchRange readings = Rs.toList searchRange & concatMap findInRow & listToMaybe 
    where findInRow row = rowBlanks row searchRange readings & Rs.toList & map (,row)


solve1 :: Input -> Int
solve1 readings = tilesCovered - beacons
    where rowOfInterest = 2000000
          tilesCovered = rowCoverage rowOfInterest readings & Rs.size
          beacons = readings & map closestBeacon & filter (\point -> snd point == rowOfInterest) & countUnique

solve2 :: Input -> Int
solve2 = findBlank searchRange ▷ fromJust ▷ tuningFrequency
    where searchRange = Rs.singletonRange (0, 4000000)
          tuningFrequency (x, y) = 4000000 * x + y


lineParser :: Parser Reading
lineParser = do
    sensor <- string "Sensor at " >> pointParser
    string ": "
    closestBeacon <- string "closest beacon is at " >> pointParser
    return Reading { sensor, closestBeacon }

pointParser :: Parser Point
pointParser = do
    x <- string "x=" >> signed (return ()) decimal
    string ", "
    y <- string "y=" >> signed (return ()) decimal
    return (x, y)

parser :: Parser Input
parser = lineParser `sepBy` newline

run :: String -> [Int]
run = applyParser parser ▷ applyEach [solve1, solve2]
