import Data.List (sort, tails)

import Runner (runner)

{-|
   Solver for Day 10 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/10
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let diffs = joltageDiffs input
  in length (filter (== 1) diffs) * length (filter (== 3) diffs)

solve2 :: String -> Int
solve2 = countCompositions . joltageDiffs

joltageDiffs :: String -> [Int]
joltageDiffs input =
  let
    adapters = sort $ map read $ lines input
    joltages = [0] <> adapters <> [last adapters + 3]
  in zipWith (-) (tail joltages) joltages

countCompositions :: [Int] -> Int
countCompositions ds = case ds of
  []  -> 1
  3:t -> countCompositions t
  _   -> let (ones, rest) = break (/= 1) ds
         in (tribonacci !! sum ones) * countCompositions rest

tribonacci :: [Int]
tribonacci = 1 : 1 : 2 : map (sum . take 3) (tails tribonacci)
