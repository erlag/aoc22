module Util where
import Data.List (sort, singleton, nub, intersperse)
import Data.List.Split (splitOn, divvy)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import qualified Data.Set as Set

-- reverse function composition, for constructing pipeline
(▷) :: (a -> b) -> (b -> c) -> a -> c
f ▷ g = g . f

-- pipeline from list of functions
pipeline :: [a -> a] -> a -> a
pipeline = foldr (▷) id

-- apply all functions in a list to the same argument
applyEach :: [a -> b] -> a -> [b]
applyEach fs x = map ($ x) fs

-- apply both functions to the same argument and produce a pair
applyBoth :: (a -> b, a -> c) -> a -> (b, c)
applyBoth (f, g) x = (f x, g x)

-- split a string at double line breaks
blocks :: String -> [String]
blocks = splitOn "\n\n"

-- use given separator to join a list of strings
joinBy :: String -> [String] -> String
joinBy sep = intersperse sep ▷ concat

-- split string by whitespace and parse each word according to the desired type (e.g. Integer)
readWords :: Read a => String -> [a]
readWords = words ▷ map read

-- parse each character according to the desired type (e.g. Integer)
readChars :: Read a => String -> [a]
readChars = map singleton ▷ map read

-- sort in descending order
sortDesc :: Ord a => [a] -> [a]
sortDesc = sort ▷ reverse

-- convert a list of to elements to a pair
asPair :: [b] -> (b, b)
asPair [a, b] = (a, b)

-- apply a pair of function to respective values
mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) = bimap f g

-- apply a function to both elements
both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- combine pair elements... pairwise
zipPairWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipPairWith f a = mapPair (both f a)

-- elements that exist in all lists
intersection :: Ord a => [[a]] -> [a]
intersection = map Set.fromList ▷ foldr1 Set.intersection ▷ Set.toList

-- count elements where predicate is true
count :: (a -> Bool) -> [a] -> Integer
count condition = filter condition ▷ length ▷ fromIntegral

-- count number of unique elements
countUnique :: Eq a => [a] -> Integer
countUnique = nub ▷ length ▷ fromIntegral

-- return value in case of Just value, or raise an error with message in case of None
orError :: String -> Maybe a -> a
orError msg = fromMaybe (error msg)

-- like zip but pad with Nothing when one list is shorter than the other
padZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
padZip [] [] = []
padZip [] bs = [(Nothing, Just b) | b <- bs]
padZip as [] = [(Just a, Nothing) | a <- as]
padZip (a:as) (b:bs) = (Just a, Just b) : padZip as bs

fullChunksOf :: Int -> [a] -> [[a]]
fullChunksOf n = divvy n n
