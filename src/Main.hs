module Main (main) where
import System.Environment ( getArgs )
import Control.Exception ( evaluate, handle, handleJust, IOException, ErrorCall, Exception )
import Control.Monad (when)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Util ( (▷), orError, padZip )
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import Data.Maybe (catMaybes)

type Solution = String -> [String]

data Assessment = Missing | New | Correct | Wrong deriving (Eq, Show)

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

invoke :: (a -> [String]) -> IO a -> IO String
invoke !solution inputAction = do
    input <- inputAction
    maybeOutputs <- mapM maybeUndefined $ solution input
    return $ unlines $ catMaybes maybeOutputs

check :: (String, Solution) -> IO ()
check (name, solution) = do
    let inputFile =  "./inputs/" ++ name ++ ".txt"
    let outputFile = "./outputs/" ++ name ++ ".txt"
    actual <- inputFile & readFile & invoke solution <&> words
    expected <- outputFile & readFile <&> words & ioFallback []
    let assessment = padZip expected actual & map assess
    putStrLn $ showAssessment name expected actual assessment
    let shouldUpdate = New `elem` assessment && not (Missing `elem` assessment || Wrong `elem` assessment)
    when shouldUpdate $ writeFile outputFile (unlines actual)

showAssessment :: String -> [String] -> [String] -> [Assessment] -> String
showAssessment name expected actual assessment = name ++ " " ++ summary ++ " " ++ values ++ comment
    where
        summary = assessment & map (\case { Missing -> '-'; New -> '+'; Correct -> '='; Wrong -> '!' })
        values = unwords actual
        comment | hasProblem = " (expected " ++ unwords expected ++ ")"
                | otherwise = ""
        hasProblem = Missing `elem` assessment || Wrong `elem` assessment


assess :: Eq a => (Maybe a, Maybe a) -> Assessment
assess (Just _, Nothing) = Missing
assess (Nothing, Just _) = New
assess (Just a, Just b) | a == b = Correct | otherwise = Wrong


ioFallback :: a -> IO a -> IO a
ioFallback value = handle (\case (err::IOException) -> return value)

fallback :: Exception e => (e -> Bool) -> a -> IO a -> IO a
fallback p x = handleJust (\e -> if p e then Just x else Nothing) return

isUndefinedError :: ErrorCall -> Bool
isUndefinedError (e::ErrorCall) = "Prelude.undefined" `isPrefixOf` show e

maybeUndefined :: a -> IO (Maybe a)
maybeUndefined x = fallback isUndefinedError Nothing (Just <$> evaluate x)
