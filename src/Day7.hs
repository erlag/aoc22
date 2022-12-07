{- Day 7: No Space Left On Device [https://adventofcode.com/2022/day/7]

1. Find all of the directories with a total size of at most 100000. What is the sum of the total sizes of those directories?
2. To run the update, you need unused space of at least 30000000. Find the smallest directory that, if deleted, would free up enough space on the filesystem to run the update. What is the total size of that directory?

Example:
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
=> 95437 24933642
-}

module Day7 (run) where
import Util ( (▷), applyEach )
import Data.Function ((&))
import GHC.IO.Device (IODeviceType(Directory))


type Path = [Name]
type Name = String
type Size = Integer
data Node = File Name Size | Dir Name [Node] Size

size :: Node -> Size
size (File _ s) = s
size (Dir _ _ s) = s

walk :: Node -> [Node]
walk f@(File {} ) = [f]
walk d@(Dir name entries _) = d : concatMap walk entries

isDir :: Node -> Bool
isDir Dir {} = True
isDir _ = False

dirSizes :: Node -> [Size]
dirSizes = walk ▷ filter isDir ▷ map size


solve1 :: Node -> Integer
solve1 = dirSizes ▷ filter (<= 100000) ▷ sum

solve2 :: Node -> Integer
solve2 root =
    let total = 70000000
        required = 30000000
        used = size root
        free = total - used
        missing = required - free
    in root & dirSizes & filter (>= missing) & minimum


parse :: String -> Node
parse = lines ▷ map words ▷ parseTree
    where
        parseTree r = root
            where ([root], []) = parseDirs r

        parseDirs [] = ([], [])
        parseDirs (["$", "cd", ".."] : r) = ([], r)
        parseDirs (["$", "cd", name] : r0) =
            let (dir, r1) = parseDir name r0
                (dirs, r2) = parseDirs r1
            in (dir : dirs, r2)

        parseDir name (["$", "ls"] : r0) =
            let (files, r1) = parseFiles r0
                (subdirs, r2) = parseDirs r1
                entries = files ++ subdirs
            in (Dir name entries (sum $ map size entries), r2)

        parseFiles [] = ([], [])
        parseFiles r@(("$" : _) : _) = ([], r)
        parseFiles (["dir", _] : r) = parseFiles r
        parseFiles ([size, name] : r0) =
            let file = File name (read size)
                (files, r1) = parseFiles r0
            in (file : files, r1)

run :: String -> [Integer]
run = parse ▷ applyEach [solve1, solve2]
