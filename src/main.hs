module Main where
import System.Environment
import qualified Day1
import Util

type Solution = String -> [Integer]

main :: IO ()
main = do
    [name] <- getArgs
    invoke $ solution name

solution :: String -> Solution
solution "day1" = Day1.run

invoke :: Solution -> IO ()
invoke solution = interact (solution ▷ showLines)
