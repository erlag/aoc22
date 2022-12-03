module Util where
import Data.List (sort)
import Data.List.Split (splitOn)

-- reverse function composition, for constructing pipeline
(▷) :: (a -> b) -> (b -> c) -> a -> c
f ▷ g = g . f

-- apply all functions in a list to the same argument
applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($ x) fs

-- split a string at double line breaks
blocks :: String -> [String]
blocks = splitOn "\n\n"

-- split string by whitespace and parse each word according to the desired type (e.g. Integer)
readWords :: Read a => String -> [a]
readWords = words ▷ map read

-- sort in descending order
sortDesc :: Ord a => [a] -> [a]
sortDesc = sort ▷ reverse

-- turn list into string with one element per line
showLines :: Show a => [a] -> String
showLines = map show ▷ unlines

-- convert a list of to elements to a pair
asPair :: [b] -> (b, b)
asPair [a, b] = (a, b)

-- apply a pair of function to respective values
mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) (x, y) = (f x, g y)
