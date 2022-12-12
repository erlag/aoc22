module Main (main) where
import Data.List.Split (divvy)
import Data.Function ((&))
import Data.Bool (bool)

main = interact $
    lines ▷ concatMap ops ▷ scanl (&) 1 ▷ zipWith sprite (cycle [0..39]) ▷ divvy 40 40 ▷ unlines
    where ops = words ▷ \case { ["noop"] -> [(+0)] ; ["addx", v] -> [(+0), (+read v)] }
          sprite x = (`elem` [x-1 .. x+1]) ▷ bool '.' '#'
          f ▷ g = g . f
