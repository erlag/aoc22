{- Day 16: Proboscidea Volcanium [https://adventofcode.com/2022/day/16]

You start at valve AA.
All of the valves begin closed.
It will take you one minute to open a single valve,
and one minute to follow any tunnel from one valve to another.

1. Work out the steps to release the most pressure in 30 minutes. What is the most pressure you can release?
2. With you and an elephant working together for 26 minutes, what is the most pressure you could release?

Example:
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
=> 1651 1707
-}

module Day16 (run) where
import Util
import Data.Function ((&))
import Data.List (permutations, delete)
import Algorithm.Search (bfs)
import Control.Monad ( replicateM )
import Data.Map (Map, fromList, (!), keys)
import Text.Megaparsec ( sepBy, (<|>), takeP )
import Text.Megaparsec.Char ( newline, string, upperChar )
import Text.Megaparsec.Char.Lexer ( decimal )
import Data.Foldable (minimumBy)
import Debug.Trace (traceShowId)

type Name = String
data Valve = Valve { name :: Name, flowRate :: Int, neighbors :: [Name] }  deriving Show
type Input = Map Name Valve

data Agent = Agent { timeRemaining :: Int, location :: Name }

shortestPathLength :: (Name -> [Name]) -> Name -> Name -> Int
shortestPathLength neighborsLookup source destination = length path where
    Just path = bfs neighborsLookup (== destination) source

singleSourcePathLengths :: (Name -> [Name]) -> String -> [String] -> Map String Int
singleSourcePathLengths neighbors source destinations =
    fromList [(destination, shortestPathLength neighbors source destination) | destination <- destinations]
    -- note: this could be done with a single breadth-first traversal instad of doing it once per destination.
    --       but the bfs algorithm in Algorithm.Search only supports a single target.

allPairsPathLengths :: (Name -> [Name]) -> [Name] -> [Name] -> Map String (Map String Int)
allPairsPathLengths neighbors sources destinations =
    fromList [(source, singleSourcePathLengths neighbors source destinations) | source <- sources]

maximizeGain :: (Name -> Int) -> (Name -> Name -> Int) -> [Agent] -> [Name] -> Int
maximizeGain gainRateLookup delayLookup agents remainingNodes
    | null agents || null remainingNodes = 0
    | otherwise = maximum $ do
        let currentAgent : otherAgents = sortDescOn timeRemaining agents
        nextNodeChoice <- remainingNodes
        let nextDelay = delayLookup (location currentAgent) nextNodeChoice
            timeRemainingAfterDelay = timeRemaining currentAgent - nextDelay
        if timeRemainingAfterDelay < 0 then
            return $ maximizeGain gainRateLookup delayLookup otherAgents remainingNodes
        else do
            let movedAgent = currentAgent { timeRemaining = timeRemainingAfterDelay, location = nextNodeChoice }
                restOfNodes = delete nextNodeChoice remainingNodes
                gainFromMove = timeRemainingAfterDelay * gainRateLookup nextNodeChoice
                gainFromRest = maximizeGain gainRateLookup delayLookup (movedAgent : otherAgents) restOfNodes
            return (gainFromMove + gainFromRest)

solve1 :: Input -> Int
solve1 input = maximizeGain rateLookup delayLookup agents nonZeroValves
    where rateLookup name = input ! name & flowRate
          neighborsLookup name = input ! name & neighbors
          nonZeroValves = input & keys & filter (\name -> rateLookup name > 0)
          pathLengths = allPairsPathLengths neighborsLookup (startLocation:nonZeroValves) nonZeroValves
          delayLookup a b = 1 + pathLengths ! a ! b
          timeLimit = 30
          startLocation = "AA"
          agents = [Agent { timeRemaining = timeLimit, location = startLocation }]

solve2 :: Input -> Int
solve2 input = maximizeGain rateLookup delayLookup agents nonZeroValves
    where rateLookup name = input ! name & flowRate
          neighborsLookup name = input ! name & neighbors
          nonZeroValves = input & keys & filter (\name -> rateLookup name > 0)
          pathLengths = allPairsPathLengths neighborsLookup (startLocation:nonZeroValves) nonZeroValves
          delayLookup a b = 1 + pathLengths ! a ! b
          timeLimit = 26
          startLocation = "AA"
          agents = [Agent { timeRemaining = timeLimit, location = startLocation },
                    Agent { timeRemaining = timeLimit, location = startLocation }]

lineParser :: Parser Valve
lineParser = do
    name <- string "Valve " >> nameParser
    flowRate <- string " has flow rate=" >> decimal
    string "; "
    neighbors <- (string "tunnels lead to valves " <|> string "tunnel leads to valve ") >> nameParser `sepBy` string ", "
    return Valve { name, flowRate, neighbors }
    where
        nameParser :: Parser String
        nameParser = replicateM 2 upperChar

parser :: Parser Input
parser = do
    valves <- lineParser `sepBy` newline
    return $ fromList [(name valve, valve) | valve <- valves]

run :: String -> [Int]
run = applyParser parser ▷ applyEach [solve1, solve2]
