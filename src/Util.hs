module Util where
import Data.List (sort, sortOn, singleton, nub, intersperse)
import Data.List.Split (splitOn, divvy)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Data.Bifunctor (bimap)
import Data.Void (Void)
import Control.Exception (Exception, throw)
import qualified Data.Set as S
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC

-- reverse function composition, for constructing pipeline
(▷) :: (a -> b) -> (b -> c) -> a -> c
f ▷ g = g . f

-- pipeline from list of functions
pipeline :: [a -> a] -> a -> a
pipeline = foldr (▷) id

-- apply function repeatedly until output is equal to input
fix' :: Eq t => (t -> t) -> t -> t
fix' f x = let y = f x in if y == x then y else fix' f y

-- apply function repeatedly until output is equal to input, or Nothing
maybeFix :: Eq a => (a -> Maybe a) -> a -> Maybe a
maybeFix f x = fix' (>>= f) (Just x)

-- iterate function, calling it with its own output, until it returns Nothing, and return list of produced values
iterJust :: (t -> Maybe t) -> t -> [t]
iterJust f x = x : case f x of Nothing -> []; Just y -> iterJust f y

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

-- rotoate list left
rotLeft :: [a] -> [a]
rotLeft [] = []
rotLeft l = tail l ++ [head l]

-- sort in descending order
sortDesc :: Ord a => [a] -> [a]
sortDesc = sort ▷ reverse

-- sort in descending order
sortDescOn :: Ord b => (a -> b) -> [a] -> [a]
sortDescOn f = sortOn f ▷ reverse

-- convert a list of to elements to a pair
asPair :: [a] -> (a, a)
asPair [x, y] = (x, y)

-- apply a pair of function to respective values
mapPair :: (a -> c, b -> d) -> (a, b) -> (c, d)
mapPair (f, g) = bimap f g

-- apply a function to both elements
both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

-- combine pair elements... pairwise
zipPairWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipPairWith f a = mapPair (both f a)

-- add 2-tuples elementwise (e.g. to move a 2D point)
add2 :: Num a => (a, a) -> (a, a) -> (a, a)
add2 = zipPairWith (+)

-- 2-tuple subtraction elementwise (e.g. as for determining distance between 2D points)
sub2 :: Num a => (a, a) -> (a, a) -> (a, a)
sub2 = zipPairWith (-)

-- elements that exist in all lists
intersection :: Ord a => [[a]] -> [a]
intersection = map S.fromList ▷ foldr1 S.intersection ▷ S.toList

-- count elements where predicate is true
count :: Integral i => (a -> Bool) -> [a] -> i
count condition = filter condition ▷ length ▷ fromIntegral

-- count number of unique elements
countUnique :: (Eq a, Integral i) => [a] -> i
countUnique = nub ▷ length ▷ fromIntegral

findDuplicates :: (Ord a) => [a] -> S.Set a -> [a]
findDuplicates [] _ = []
findDuplicates (h:t) seen | S.member h seen = h : findDuplicates t seen 
                          | otherwise         = findDuplicates t (S.insert h seen)

-- return value in case of Just value, or raise an error with message in case of None
orError :: String -> Maybe a -> a
orError msg = fromMaybe (error msg)

-- apply function to adjacent pairs of elements
mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

-- like zip but pad with Nothing when one list is shorter than the other
padZip :: [a] -> [b] -> [(Maybe a, Maybe b)]
padZip [] [] = []
padZip [] bs = [(Nothing, Just b) | b <- bs]
padZip as [] = [(Just a, Nothing) | a <- as]
padZip (a:as) (b:bs) = (Just a, Just b) : padZip as bs

-- Zip elements in two two-dimensional lists
zip2D :: [[a]] -> [[b]] -> [[(a, b)]]
zip2D = zipWith zip

-- split into chunks of specified size -- returning only full chunks
fullChunksOf :: Int -> [a] -> [[a]]
fullChunksOf n = divvy n n


-- Megaparsec parser type with String input and no custom error type
type Parser = P.Parsec Void String
type ParserError = P.ParseErrorBundle String Void

-- run Megaparsec parser, consuming all input except trailing whitespace, and returning result, throwing error on failure
applyParser :: Parser a -> String -> a
applyParser p s = P.runParser (p <* endinput) "" s & rightOrError
    where endinput = P.many PC.newline >> P.eof

-- return value if Right, or throw exception from Left
rightOrError :: Exception e => Either e a -> a
rightOrError (Left e)  = throw e
rightOrError (Right a) = a
