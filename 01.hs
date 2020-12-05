import Data.List (tails)

import Runner (runner)

{-|
  Solver for Day 1 of the Advent of Code 2020
  Problem description: https://adventofcode.com/2020/day/1
-}

main :: IO ()
main = runner (solve 2) (solve 3)

solve :: Int -> [Char] -> Int
solve n input =
  let report = map read $ lines input
  in product $ findNums n report

findNums :: Int -> [Int] -> [Int]
findNums n = head . filter ((== 2020) . sum) . combinations n

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = do
  y:ys <- tails xs
  cs <- combinations (n-1) ys
  return (y:cs)
