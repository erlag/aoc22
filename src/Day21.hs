{- Day 21: Monkey Math [https://adventofcode.com/2022/day/21]

1. What number will the monkey named root yell?

2. Ignore the operator assigned for root and treat it as = i.e. equality check instead. Also ignore the given number for "humn". What number does "humn" need to have so that root's equality check passes?

Example:
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
=> 152 301
-}

module Day21 (run) where
import Util ( Parser, (▷), applyEach, applyParser )
import Data.Functor ( (<&>) )
import Data.Maybe ( fromJust )
import Data.Function ( (&) )
import qualified Data.Map as M
import Text.Megaparsec ( sepBy, (<|>), try, some )
import Text.Megaparsec.Char ( space, string, newline, letterChar )
import Text.Megaparsec.Char.Lexer ( decimal )

type Input = M.Map Name Expr
type Name = String
data Expr = Const Number | Calc Name Op Name  deriving (Show)
data Op = Add | Sub | Mul | Div  deriving (Show)
type Number = Int

calc :: Op -> Number -> Number -> Number
calc Add = (+)
calc Sub = (-)
calc Mul = (*)
calc Div = div

inv1 :: Op -> Number -> Number -> Number
inv1 Add = (-)      -- x + a = b ; x = b - a
inv1 Sub = (+)      -- x - a = b ; x = b + a
inv1 Mul = div      -- x * a = b ; x = b / a
inv1 Div = (*)      -- x / a = b ; x = b * a

inv2 :: Op -> Number -> Number -> Number
inv2 Add = (-)      -- a + x = b ; x = b - a
inv2 Sub = flip (-) -- a - x = b ; a = b + x; x = a - b
inv2 Mul = div      -- a * x = b ; x = b / a
inv2 Div = flip div -- a / x = b ; a = b * x; x = a / b


eval :: Name -> M.Map Name Expr -> Maybe Number
eval name assignments = evalName name where
    evalName name = M.lookup name assignments >>= evalExpr
    evalExpr (Const x)     = Just x
    evalExpr (Calc a op b) = calc op <$> evalName a <*> evalName b

solve1 :: Input -> Number
solve1 = eval "root" ▷ fromJust


solveEqUnknown :: Name -> Name -> M.Map Name Expr -> Maybe Number
solveEqUnknown a b assignments = eqNames a b where
    eqNames a b = case (eval a assignments, eval b assignments) of
        (Nothing, Nothing) -> Nothing -- can't easily solve when there's an unknown on both sides
        (Just _, Just _)   -> Nothing -- there's no unknown that needs solving here
        (Just x, Nothing)  -> eqName b x
        (Nothing, Just x)  -> eqName a x

    eqName name val = case M.lookup name assignments of Nothing -> Just val; Just expr -> eqExpr expr val

    eqExpr (Const _) val = Nothing -- there's no unknown that needs solving here
    eqExpr (Calc a op b) val = case (eval a assignments, eval b assignments) of
        (Nothing, Nothing) -> Nothing -- can't easily solve when there's an unknown on both sides
        (Just _, Just _)   -> Nothing -- there's no unknown that needs solving here
        (Just a', Nothing) -> eqName b (inv2 op val a')
        (Nothing, Just b') -> eqName a (inv1 op val b')

solve2 :: Input -> Number
solve2 assignments = solveEqUnknown a b assignmentsWithUnknown & fromJust
    where assignmentsWithUnknown = M.delete "humn" assignments
          Calc a _ b = assignments M.! "root"


exprParser :: Parser Expr
exprParser = (Const <$> decimal) <|> (Calc <$> nameParser <*> opParser <*> nameParser)

nameParser :: Parser Name
nameParser = some letterChar

opParser :: Parser Op
opParser = (string " + " >> return Add) <|> (string " - " >> return Sub) <|> (string " * " >> return Mul) <|> (string " / " >> return Div)

assignmentParser :: Parser (Name, Expr)
assignmentParser = do name <- nameParser; string ": "; expr <- exprParser; return (name, expr)

parser :: Parser Input
parser = assignmentParser `sepBy` newline <&> M.fromList

run :: String -> [Number]
run = applyParser parser ▷ applyEach [solve1, solve2]
