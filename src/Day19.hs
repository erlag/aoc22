{- Day 19: Not Enough Minerals [https://adventofcode.com/2022/day/19]

Each robot can collect 1 of its resource type per minute. 
It also takes one minute for the robot factory (also conveniently from your pack) to construct any type of robot,
 although it consumes the necessary resources available when construction begins.

Determine the quality level of each blueprint by multiplying that blueprint's ID number with the largest number of geodes that can be opened in 24 minutes using that blueprint. 

1. What do you get if you add up the quality level of all of the blueprints in your list?
2. Don't worry about quality levels; instead, just determine the largest number of geodes you could open using each of the first three blueprints in 32 minutes. What do you get if you multiply these numbers together?

Example:
Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
=> 33
-}

module Day19 (run) where
import Util
import Data.Function ( (&) )
import Text.Megaparsec ( sepBy, (<|>), try )
import Text.Megaparsec.Char ( space, string )
import Text.Megaparsec.Char.Lexer ( decimal )
import Data.Maybe ( catMaybes, mapMaybe, fromMaybe )
import Debug.Trace (trace, traceShow, traceShowId)

data Resources = Resources { ore :: Int, clay :: Int, obsidian :: Int, geode :: Int }  deriving (Show)
data Blueprint = Blueprint { blueprintId :: Int, oreRobotCost :: Resources, clayRobotCost :: Resources, obsidianRobotCost :: Resources, geodeRobotCost :: Resources }
type Input = [Blueprint]

--data Resource = Ore | Clay | Obsidian | Geode
--type ResourceAmount = (Int, Resource)
--data RobotType = Robot { produces :: Resource, costs :: [ResourceAmount] }

zero :: Resources
zero = Resources { ore = 0, clay = 0, obsidian = 0, geode = 0 }

add :: Resources -> Resources -> Resources
add a b = Resources { ore = ore a + ore b, clay = clay a + clay b, obsidian = obsidian a + obsidian b, geode = geode a + geode b }

times :: Int -> Resources -> Resources
times n a = Resources { ore = n * ore a, clay = n * clay a, obsidian = n * obsidian a, geode = n * geode a }

sub :: Resources -> Resources -> Resources
sub a b = Resources { ore = ore a - ore b, clay = clay a - clay b, obsidian = obsidian a - obsidian b, geode = geode a - geode b }

addOre :: Resources -> Resources
addOre = (`add` Resources { ore = 1, clay = 0, obsidian = 0, geode = 0 })

addClay :: Resources -> Resources
addClay = (`add` Resources { ore = 0, clay = 1, obsidian = 0, geode = 0 })

addObsidian :: Resources -> Resources
addObsidian = (`add` Resources { ore = 0, clay = 0, obsidian = 1, geode = 0 })

addGeode :: Resources -> Resources
addGeode = (`add` Resources { ore = 0, clay = 0, obsidian = 0, geode = 1 })

stepsUntilNeedMet :: Int -> Int -> Maybe Int
stepsUntilNeedMet need production | need <= 0       = Just 0
                                  | production == 0 = Nothing
                                  | otherwise       = Just $ (need + production - 1) `div` production

stepsToBuild :: Resources -> Resources -> Resources -> Maybe Int
stepsToBuild cost available production = 
    let need = cost `sub` available in do
        oreSteps <- stepsUntilNeedMet (ore need) (ore production)
        claySteps <- stepsUntilNeedMet (clay need) (clay production)
        obsidianSteps <- stepsUntilNeedMet (obsidian need) (obsidian production)
        geodeSteps <- stepsUntilNeedMet (geode need) (geode production)
        return $ 1 + (oreSteps `max` claySteps `max` obsidianSteps `max` geodeSteps)

maxGeodes :: Blueprint -> Resources -> Resources -> Int -> Int
maxGeodes blueprint = loop where 
    loop resources production stepsLeft = 
        let
            evaluateAlternative robotCost productionChange = 
                case stepsToBuild robotCost resources production of
                    Just n | n < stepsLeft ->
                        Just $ loop (resources `sub` robotCost `add` times n production) (productionChange production) (stepsLeft - n)
                    _ ->
                        Nothing
            oreRobotResult = evaluateAlternative (oreRobotCost blueprint) addOre
            clayRobotResult = evaluateAlternative (clayRobotCost blueprint) addClay
            obsidianRobotResult = evaluateAlternative (obsidianRobotCost blueprint) addObsidian
            geodeRobotResult = evaluateAlternative (geodeRobotCost blueprint) addGeode
            bestResult = oreRobotResult `max` clayRobotResult `max` obsidianRobotResult `max` geodeRobotResult
            fallback = geode resources + stepsLeft * geode production
        in bestResult & fromMaybe fallback

solve1 :: Input -> Int
solve1 blueprints = 
    let initialProduction = Resources { ore = 1, clay = 0, obsidian = 0, geode = 0 }
        steps = 24
    in blueprints & map (\blueprint -> blueprintId blueprint * maxGeodes blueprint zero initialProduction steps) & sum

solve2 :: Input -> Int
solve2 blueprints =
    let initialProduction = Resources { ore = 1, clay = 0, obsidian = 0, geode = 0 }
        steps = 32
    in take 3 blueprints & map (\blueprint -> maxGeodes blueprint zero initialProduction steps) & sum


resourceCostsParser :: Parser Resources
resourceCostsParser = do
    ore <- try (decimal <* string " ore") <|> return 0
    string " and " <|> return ""
    clay <- try (decimal <* string " clay") <|> return 0
    string " and " <|> return ""
    obsidian <- (decimal <* string " obsidian") <|> return 0
    return Resources { ore, clay, obsidian, geode = 0 }

blueprintParser :: Parser Blueprint
blueprintParser = do
    blueprintId <- string "Blueprint " *> decimal <* string ":"
    space
    oreRobotCost <- string "Each ore robot costs " *> resourceCostsParser <* string "."
    space
    clayRobotCost <- string "Each clay robot costs " *> resourceCostsParser <* string "."
    space
    obsidianRobotCost <- string "Each obsidian robot costs " *> resourceCostsParser <* string "."
    space
    geodeRobotCost <- string "Each geode robot costs " *> resourceCostsParser <* string "."
    return Blueprint { blueprintId, oreRobotCost, clayRobotCost, obsidianRobotCost, geodeRobotCost }

parser :: Parser Input
parser = blueprintParser `sepBy` space

run :: String -> [Int]
run = applyParser parser ▷ applyEach [solve1] --, solve2]
