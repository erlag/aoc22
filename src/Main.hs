module Main where
import System.Environment
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

import Util

type Solution = String -> [Integer]

main :: IO ()
main = do
    [name] <- getArgs
    invoke $ solution name

solution :: String -> Solution
solution "day1" = Day1.run
solution "day2" = Day2.run
solution "day3" = Day3.run
solution "day4" = Day4.run

invoke :: Solution -> IO ()
invoke solution = interact (solution ▷ showLines)
