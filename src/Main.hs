module Main (main) where
import System.Environment ( getArgs )
import Control.Exception ( catch, IOException )
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor ((<&>))
import Util ( (▷), orError, padZip )
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

type Solution = String -> [String]

data CheckOutcome = Missing | New | Correct | Wrong deriving (Eq, Show)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> mapM_ check solutions
        [name] -> check (name, getSolution name)
        [name, "-"] -> invoke (getSolution name) getContents >>= putStr
        [name, infile] -> invoke (getSolution name) (readFile infile) >>= putStr
        [name, infile, outfile] -> invoke (getSolution name) (readFile infile) >>= writeFile outfile
        _ -> error "usage: aoc22 [day [- | infile [outfile]]]"

getSolution :: String -> Solution
getSolution name = lookup name solutions & orError ("no solution for " ++ name)


solutions :: [(String, Solution)] 
solutions = 
    [ ("day1", Day1.run ▷ map show)
    , ("day2", Day2.run ▷ map show)
    , ("day3", Day3.run ▷ map show)
    , ("day4", Day4.run ▷ map show)
    , ("day5", Day5.run)
    , ("day6", Day6.run ▷ map show)
    , ("day7", Day7.run ▷ map show)
    , ("day8", Day8.run ▷ map show)
    ]

invoke :: Solution -> IO String -> IO String
invoke !solution = fmap (solution ▷ unlines)

check :: (String, Solution) -> IO ()
check (name, solution) = do
    let inputFile =  "./inputs/" ++ name ++ ".txt"
    let outputFile = "./outputs/" ++ name ++ ".txt"
    actual <- inputFile & readFile & invoke solution <&> words
    expected <- (outputFile & readFile <&> words) `orFallback` []
    let evaluation = padZip expected actual & map evaluate
    putStrLn $ showEvaluation name expected actual evaluation
    let shouldUpdate = New `elem` evaluation && not (Missing `elem` evaluation || Wrong `elem` evaluation)
    when shouldUpdate $ writeFile outputFile (unlines actual)

showEvaluation :: String -> [String] -> [String] -> [CheckOutcome] -> String
showEvaluation name expected actual evaluation = name ++ " " ++ evaluationSummary ++ " " ++ values ++ comment
    where
        evaluationSummary = evaluation & map (\case { Missing -> '-'; New -> '+'; Correct -> '='; Wrong -> '!' })
        values = unwords actual
        comment | hasProblem = " (expected " ++ unwords expected ++ ")"
                | otherwise = ""
        hasProblem = Missing `elem` evaluation || Wrong `elem` evaluation

orFallback :: IO a -> a -> IO a
task `orFallback` value = catch task (\case (err::IOException) -> return value)

evaluate :: Eq a => (Maybe a, Maybe a) -> CheckOutcome
evaluate (Just _, Nothing) = Missing
evaluate (Nothing, Just _) = New
evaluate (Just a, Just b) | a == b = Correct | otherwise = Wrong
