module Main (main) where
import Util ( (▷), applyEach, blocks, readWords, sortDesc )

main = interact $ 
    blocks ▷ map (readWords ▷ sum) ▷ applyEach [ maximum, sortDesc ▷ take 3 ▷ sum ] ▷ map show ▷ unlines
