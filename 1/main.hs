import Data.List (tails)
import System.Environment (getArgs)

{-
   Solver for Day 1 of the Advent of Code 2020
   Problem description here: https://adventofcode.com/2020/day/1

   Takes a command line argument indicating which part to solve (1 or 2), reads
   input from stdin, and prints solution to stdout.
-}

main :: IO ()
main = do
  let run = (>>= putStrLn) . (<$> getContents) . (show .)
  part <- getArgs
  case part of
    ["1"]     -> run $ solve 2
    ["2"]     -> run $ solve 3
    otherwise -> putStrLn "Valid options are 1 or 2"

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
