{- Day 11: Monkey in the Middle [https://adventofcode.com/2022/day/11]

The monkeys take turns inspecting and throwing items.
On a single monkey's turn, it inspects and throws all of the items it is holding one at a time and in the order listed.
Monkey 0 goes first, then monkey 1, and so on until each monkey has had one turn.
The process of each monkey taking a single turn is called a round.

Each item has a worry level indicating how worried you are for this item.
Each monkey has an Operation specifying how the worry level for an item changes as that monkey inspects an item.
Each monkey has a Test specifying how the monkey uses your worry level to decide where to throw an item next.
When a monkey throws an item to another monkey, the item goes on the end of the recipient monkey's list.

Count the total number of times each monkey inspects items.
Focus on the two most active monkeys. The level of monkey business is found by multiplying together the number of times they inspected items.

1. What is the level of monkey business after 20 rounds? After a monkey inspects an item but before it tests your worry level, your worry level is be divided by three and rounded down to the nearest Integereger.
2. What is the level of monkey business after 10000 rounds? Worry levels are no longer divided by three after each item is inspected.

Example:
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
=> 10605 2713310158
-}

module Day11 (run) where
import Util ( applyEach, (▷), Parser, pipeline, applyBoth, sortDesc, applyParser )
import qualified Data.Map as M
import Data.Function ((&))
import Text.Megaparsec ( sepBy, (<|>) )
import Text.Megaparsec.Char ( newline, string )
import Text.Megaparsec.Char.Lexer (decimal)

-- Input types
type Input = [MonkeySpec]
type MonkeyId = Int
data MonkeySpec = MonkeySpec { monkeyId :: MonkeyId
                             , startingItems :: [Integer]
                             , operation :: Operation
                             , testDivisor :: Integer
                             , targetIfTrue :: MonkeyId
                             , targetIfFalse :: MonkeyId
                            } deriving (Show)
data Operator = Add | Mul  deriving (Show)
data Operand = Old | Const Integer  deriving (Show)
data Operation = Op Operand Operator Operand  deriving (Show)

-- Simulation types
type State = M.Map MonkeyId Monkey
data Monkey = Monkey { items :: [Integer]
                     , numInspected :: Integer
                     , inspect :: Integer -> ThrowItem
                     }
data ThrowItem = ThrowItem MonkeyId Integer


-- Simulation
rounds :: Int -> State -> State
rounds n = pipeline $ replicate n singleRound

singleRound :: State -> State
singleRound monkeys = pipeline turns monkeys where turns = map turnFor $ M.keys monkeys

turnFor :: MonkeyId -> State -> State
turnFor monkeyId monkeys = let
    (monkey', throws) = takeTurn (monkeys M.! monkeyId)
    monkeys' = M.insert monkeyId monkey' monkeys
    in foldl (flip receiveThrow) monkeys' throws

takeTurn :: Monkey -> (Monkey, [ThrowItem])
takeTurn monkey = let throws = map (inspect monkey) (items monkey)
                      numThrows = fromIntegral $ length throws
                  in (monkey { items = [], numInspected = numInspected monkey + numThrows }, throws)

receiveThrow :: ThrowItem -> State -> State
receiveThrow (ThrowItem monkeyId item) = M.adjust (addItem item) monkeyId

addItem :: Integer -> Monkey -> Monkey
addItem item monkey = monkey { items = items monkey ++ [item] }

measureMonkeyBusiness :: State -> Integer
measureMonkeyBusiness = M.elems ▷ map numInspected ▷ sortDesc ▷ take 2 ▷ product


-- Initialization
initialize :: (Integer -> Integer) -> [MonkeySpec] -> State
initialize postUpdateOp = map (applyBoth (monkeyId, mkMonkey postUpdateOp)) ▷ M.fromList

mkMonkey :: (Integer -> Integer) -> MonkeySpec -> Monkey
mkMonkey postUpdateOp MonkeySpec { startingItems, operation, testDivisor, targetIfTrue, targetIfFalse } =
    Monkey { items = startingItems
           , numInspected = 0
           , inspect = mkOp operation ▷ postUpdateOp ▷ mkTest testDivisor targetIfTrue targetIfFalse
           }

mkOp :: Operation -> Integer -> Integer
mkOp (Op a o b) =
    let impl = case o of Mul -> (*) ; Add -> (+)
        evalA = case a of Old -> id ; Const v -> const v
        evalB = case b of Old -> id ; Const v -> const v
    in \x -> impl (evalA x) (evalB x)

mkTest :: Integer -> MonkeyId -> MonkeyId -> Integer -> ThrowItem
mkTest divisor ifTrue ifFalse item = ThrowItem target item
    where target = if item `mod` divisor == 0 then ifTrue else ifFalse


-- Invocation
solve1 :: Input -> Integer
solve1 = initialize (`div` 3) ▷ rounds 20 ▷ measureMonkeyBusiness

solve2 :: Input -> Integer
solve2 monkeySpecs =
    let base = product $ map testDivisor monkeySpecs
    in monkeySpecs & initialize (`mod` base) & rounds 10000 & measureMonkeyBusiness

parser :: Parser [MonkeySpec]
parser = monkeySpec `sepBy` string "\n\n"
    where
        monkeySpec = do
            monkeyId <- string "Monkey " >> decimal <* string ":" <* newline
            startingItems <- string "  Starting items: " >> decimal `sepBy` string ", " <* newline
            operation <- string "  Operation: new = " >> operation <* newline
            testDivisor <- string "  Test: divisible by " >> decimal <* newline
            targetIfTrue <- string "    If true: throw to monkey " >> decimal <* newline
            targetIfFalse <- string "    If false: throw to monkey " >> decimal
            return MonkeySpec { monkeyId, startingItems, operation, testDivisor, targetIfTrue, targetIfFalse }
        operation = Op <$> operand <*> operator <*> operand
        operator = (string " * " >> return Mul) <|> (string " + " >> return Add)
        operand = (string "old" >> return Old) <|> (Const <$> decimal)

run :: String -> [Integer]
run = applyParser parser ▷ applyEach [solve1, solve2]
