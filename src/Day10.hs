{- Day 10: Cathode-Ray Tube [https://adventofcode.com/2022/day/10]

The CPU has a single register, X, which starts with the value 1.
- addx V takes two cycles to complete. After two cycles, the X register is increased by the value V.
- noop takes one cycle to complete. It has no other effect.

1. Find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles. What is the sum of these six signal strengths?

The CRT is 40 pixels wide and 6 high and draws a single pixel during each cycle. 
First the top row of pixels left-to-right, then the row below that, and so on.

The X register sets the horizontal position of the middle of a sprite which is 3 pixels wide.
If the sprite is positioned such that it overlaps the pixel currently being drawn, the screen produces a lit pixel.

2. Render the image given by your program. What eight capital letters appear on your CRT?

Example:
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
=> 
13140
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
-}

module Day10 (run) where
import Util ( (▷), applyBoth, fullChunksOf, joinBy )
import Data.Function ((&))

type Input = [Instruction]
data Instruction = AddX Int | NoOp

type State = Int
type MicroOp = Int -> Int

-- Convert instruction to a list of operations that take one cycle each
microOps :: Instruction -> [MicroOp]
microOps NoOp = [(+0)]
microOps (AddX v) = [(+0), (+v)]

initialState :: State
initialState = 1

executionTrace :: [MicroOp] -> [State]
executionTrace = scanl (&) initialState


solve1 :: [MicroOp] -> Int
solve1 = executionTrace ▷ zipWith (*) cycleNumbers ▷ fullChunksOf 40 ▷ map (!!19) ▷ sum
    where cycleNumbers = [1..]


spriteAt :: Int -> Int -> Char
spriteAt x = \case y | y `elem` pixels -> '#' | otherwise -> '.'
    where pixels = [x-1 .. x+1]

solve2 :: [MicroOp] -> String
solve2 = executionTrace ▷ map spriteAt ▷ zipWith (&) beamPos ▷ fullChunksOf 40 ▷ joinBy "\n"
    where beamPos = cycle [0..39]

simplify :: [Instruction] -> [MicroOp]
simplify = concatMap microOps

parse :: String -> Input
parse = lines ▷ map (words ▷ parseInstruction)
    where parseInstruction ["noop"] = NoOp
          parseInstruction ["addx", v] = AddX (read v)

run :: String -> (Int, String)
run = parse ▷ simplify ▷ applyBoth (solve1, solve2)
