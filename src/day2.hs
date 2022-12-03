{- Day 2: Rock Paper Scissors [https://adventofcode.com/2022/day/2]

What would your total score be if everything goes exactly according to your strategy guide?
1. Instruction represents opponent shape and my shape (Rock, Paper, Scissors)
2. Instruction represents opponent shape and desired outcome (lose, draw, win)

The score for a round is the score for the shape you selected (1 for Rock, 2 for Paper, and 3 for Scissors) 
plus the score for the outcome of the round (0 if you lost, 3 if the round was a draw, and 6 if you won).

Example:
A Y
B X
C Z
=> 15 12
-}

module Day2 (run) where
import Util ( (▷), asPair, mapPair, applyEach )

data Shape = Rock | Paper | Scissors              deriving (Eq, Show)
data Play = Play { my :: Shape, other :: Shape }  deriving (Eq, Show)
data Outcome = Loss | Draw | Win                  deriving (Eq, Show)
type Score = Integer

beaterOf :: Shape -> Shape
beaterOf = \case { Rock -> Paper; Paper -> Scissors; Scissors -> Rock }

loserOf :: Shape -> Shape
loserOf = \case { Scissors -> Paper; Paper -> Rock; Rock -> Scissors }

outcome :: Play -> Outcome
outcome play
    | my play == beaterOf (other play) = Win
    | my play == other play            = Draw
    | my play == loserOf (other play)  = Loss

strategyFor :: Outcome -> Shape -> Shape
strategyFor Win = beaterOf
strategyFor Draw = id
strategyFor Loss = loserOf

score :: Play -> Score
score play = shapeScore (my play) + outcomeScore (outcome play)

shapeScore :: Shape -> Score
shapeScore = \case Rock -> 1; Paper -> 2; Scissors -> 3

outcomeScore :: Outcome -> Score
outcomeScore = \case Loss -> 0; Draw -> 3; Win -> 6


type Instruction = (String, String)
type Input = [Instruction]

parse :: String -> Input
parse = lines ▷ map (words ▷ asPair)

interpretation1 :: Instruction -> Play
interpretation1 = mapPair (readOtherShape, readMyShape) ▷ \(other, my) -> Play { other, my }

interpretation2 :: Instruction -> (Shape, Outcome)
interpretation2 = mapPair (readOtherShape, readMyOutcome)

readOtherShape = \case { "A" -> Rock; "B" -> Paper; "C" -> Scissors }
readMyShape = \case { "X" -> Rock; "Y" -> Paper; "Z" -> Scissors }
readMyOutcome = \case { "X" -> Loss; "Y" -> Draw; "Z" -> Win }


solve1 :: Input -> Score
solve1 = map (interpretation1 ▷ score) ▷ sum

solve2 :: Input -> Score
solve2 = map (interpretation2 ▷ makePlay ▷ score ) ▷ sum
    where makePlay (other, outcome) = Play { other, my = strategyFor outcome other }

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
