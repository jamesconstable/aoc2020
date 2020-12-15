import Data.Char (isDigit)
import Data.IntMap.Strict (IntMap, (!?), empty, fromList, insert)

import Runner (runner)

{-|
   Solver for Day 15 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/15
-}

data Step = Step { turn :: !Int, prev :: !Int, seen :: !(IntMap Int) }

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = last . take 2020 . elfSequence . parseInput

solve2 :: String -> Int
solve2 = last . take 30000000 . elfSequence . parseInput

elfSequence :: [Int] -> [Int]
elfSequence start = start <> tail (map prev generate)
  where
    seen = fromList $ zip (init start) [1..]
    next (Step i p s) = Step (i+1) (maybe 0 (i-) (s !? p)) (insert p i s)
    generate = iterate next (Step (length start) (last start) seen)

parseInput :: String -> [Int]
parseInput input = case span isDigit input of
  ("", "")   -> []
  ("", _:xs) -> parseInput xs
  (n, t)     -> read n : parseInput t
