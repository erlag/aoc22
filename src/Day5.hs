{- Day 5: Supply Stacks [https://adventofcode.com/2022/day/5]

After the rearrangement procedure completes, what crate ends up on top of each stack?
1. Crates are moved one at a time
2. Moved crates stay in the same order

Example:
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
=> CMZ MCD
-}

module Day5 (run) where
import Util ( (▷), applyBoth, applyEach, blocks, asPair, mapPair, pipeline )
import Data.Function ((&))
import Data.List (transpose)
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import Data.List.Split (chop)
import qualified Data.Map.Strict as M

type Input = (Stacks, Instructions)
type Output = [Crate]

type Stacks = M.Map StackId Stack
type StackId = Int
type Stack = [Crate]
type Crate = Char

type Instructions = [Instruction]
data Instruction = Instruction { times :: Int, from :: StackId, to :: StackId }

takeFrom :: StackId -> Int -> Stacks -> ([Crate], Stacks)
takeFrom key n stacks = (removed, M.insert key remaining stacks)
    where (removed, remaining) = splitAt n $ stacks M.! key

addTo :: StackId -> [Crate] -> Stacks -> Stacks
addTo key crates = M.adjust (crates ++) key

move :: Int -> StackId -> StackId -> Stacks -> Stacks
move n source destination = takeFrom source n ▷ uncurry (addTo destination)

apply1 :: Instruction -> Stacks -> Stacks
apply1 Instruction { times, from, to } = replicate times (move 1 from to) & pipeline

apply2 :: Instruction -> Stacks -> Stacks
apply2 Instruction { times, from, to } = move times from to

solve1 :: Input -> Output
solve1 (stacks, instructions) = stacks & pipeline (map apply1 instructions) & heads

solve2 :: Input -> Output
solve2 (stacks, instructions) = stacks & pipeline (map apply2 instructions) & heads

heads :: Stacks -> [Crate]
heads = M.elems ▷ map (listToMaybe ▷ fromMaybe ' ')


parse :: String -> Input
parse = blocks ▷ asPair ▷ mapPair (parseStacks, parseInstructions)

parseStacks :: String -> Stacks
parseStacks = lines ▷ applyBoth (last ▷ parseIds, init ▷ extractTowers) ▷ zipToMap where
    parseIds = words ▷ map read
    extractTowers = map (chop extractOneCrate) ▷ transpose ▷ map catMaybes
    extractOneCrate (' ':' ':' ':rest) = (Nothing, drop 1 rest)
    extractOneCrate ('[':c:']':rest) = (Just c, drop 1 rest)
    zipToMap (keys, values) = M.fromList $ zip keys values

parseInstructions :: String -> Instructions
parseInstructions = lines ▷ map words
    ▷ map (\["move", times, "from", from, "to", to] -> (read times, read from, read to))
    ▷ map (\(times, from, to) -> Instruction {times, from, to})


run :: String -> [Output]
run = parse ▷ applyEach [solve1, solve2]
