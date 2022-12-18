{- Day 17: Pyroclastic Flow [https://adventofcode.com/2022/day/17]

Five shapes of rock fall one by one in the following order (repeated indefinitely):

####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##

The tall, vertical chamber is exactly seven units wide.
Each rock appears so that its left edge is two units away from the left wall
and its bottom edge is three units above the highest rock or the floor.

A falling rocks alternates between being pushed in the direction indicated by the next input symbol
(with input repeated indefinitely) and falling one step down, until it cannot fall further.

1. How many units tall will the tower of rocks be after 2022 rocks have stopped falling?
2. How tall will the tower be after 1000000000000 rocks have stopped?

Example:
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
=> 3068 1514285714288
-}

module Day17 (run) where
import Util ( (▷), applyEach, add2, zip2D, rotLeft, findDuplicates )
import Data.Maybe ( fromMaybe, fromJust, listToMaybe )
import Data.List ( findIndex, isPrefixOf, elemIndices, groupBy )
import Data.Function ( (&) )
import qualified Data.Set as S ( Set, empty, null, map, member, insert, intersection, union, lookupMax, fromList, fromDescList, toDescList )

data Push = L | R  deriving (Eq, Ord, Show)
type Input = [Push]

type Pos = (Int, Int)
type Shape = S.Set Pos

data State = State { structure :: Shape, shapes :: [Shape], pushes :: [Push] }

shapeOrder :: [Shape]
shapeOrder = parseShape <$>
    ["####"

     ,".#.\n\
      \###\n\
      \.#."

     ,"..#\n\
      \..#\n\
      \###"

     ,"#\n\
      \#\n\
      \#\n\
      \#"

     ,"##\n\
      \##"
     ]

parseShape :: String -> Shape
parseShape = lines ▷ reverse ▷ zip2D grid ▷ concat ▷ filter (snd ▷ (== '#')) ▷ map fst ▷ S.fromList

showShape :: Shape -> String
showShape shape = [[if S.member (row, col) shape then '#' else if row == 0 || col == 0 || col == 8 then ' ' else '.' | col <- [0..8]] | row <- [0..height shape]] & reverse & unlines

grid :: [[Pos]]
grid = [[(row, col) | col <- [0..]] | row <- [0..]]

push :: Push -> Shape -> Shape
push L = shiftShape (0, -1)
push R = shiftShape (0, 1)

fall :: Shape -> Shape
fall = shiftShape (-1, 0)

shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape delta = S.map (add2 delta)

height :: Shape -> Int
height = S.lookupMax ▷ fmap fst ▷ fromMaybe 0

topRows :: Int -> Shape -> Shape
topRows n shape = shape & S.toDescList ▷ groupBy (\a b -> fst a == fst b) ▷ take n ▷ concat ▷ S.fromDescList ▷ shiftShape (n - height shape, 0)

nonOverlapping :: Shape -> Shape -> Bool
nonOverlapping a b = S.intersection a b & S.null

leftWallCol :: Int
leftWallCol = 0

rightWallCol :: Int
rightWallCol = 8

floorRow :: Int
floorRow = 0

posInBounds :: Pos -> Bool
posInBounds (row, col) = col > leftWallCol && col < rightWallCol && row > floorRow

shapeInBounds :: Shape -> Bool
shapeInBounds = all posInBounds

possibleMove :: Shape -> Shape -> Bool
possibleMove structure shape = nonOverlapping shape structure && shapeInBounds shape

dropShape :: Shape -> (Shape, [Push]) -> (Shape, [Push])
dropShape shape (structure, pushes) =
    let (pushDirection, pushes') = (head pushes, rotLeft pushes)
        pushed = push pushDirection shape
        shape' = if possibleMove structure pushed then pushed else shape
        fallen = fall shape'
    in if possibleMove structure fallen
        then dropShape fallen (structure, pushes')
        else (S.union structure shape', pushes')

nextShape :: State -> State
nextShape State { structure, shapes, pushes } =
    let (shape, shapes') = (head shapes, rotLeft shapes)
        startPos = (4 + height structure, 3)
        placedShape = shiftShape startPos shape
        (structure', pushes') = dropShape placedShape (structure, pushes)
    in State { structure=structure', shapes=shapes', pushes=pushes' }

solve1 :: Input -> Int
solve1 = initialState ▷ iterate nextShape ▷ (!! 2022) ▷ structure ▷ height
    where initialState input = State { structure = S.empty, shapes = shapeOrder, pushes = input }

findFirstRepetition :: Ord a => [a] -> (Int, Int)
findFirstRepetition l =
    let firstDuplicate:_ = findDuplicates l S.empty
        firstIndex:secondIndex:_ = elemIndices firstDuplicate l
    in (firstIndex, secondIndex - firstIndex)

solve2 :: Input -> Int
solve2 input = let
    totalSteps = 1000000000000
    initialState = State { structure = S.empty, shapes = shapeOrder, pushes = input }
    states = iterate nextShape initialState
    nTopRows = 20 -- NOTE: arbitrary choice. works for my input, not necessarily for all. would be better to check all blocks added during a repetition.
    (repetitionStart, repetitionLength) = findFirstRepetition [(shapes, pushes, topRows nTopRows structure) | State { structure, shapes, pushes } <- states]
    numberOfRepetitions = (totalSteps - repetitionStart) `div` repetitionLength
    remainingSteps = (totalSteps - repetitionStart) `mod` repetitionLength
    heightAtStep step = states !! step & structure & height
    initialHeight = heightAtStep repetitionStart
    repeatedHeight = heightAtStep (repetitionStart + repetitionLength) - initialHeight
    remainingHeight = heightAtStep (repetitionStart + remainingSteps) - initialHeight
    in initialHeight + numberOfRepetitions * repeatedHeight + remainingHeight

parse :: String -> Input
parse = map (\case '<' -> L; '>' -> R)

run :: String -> [Int]
run = parse ▷ applyEach [solve1, solve2]
