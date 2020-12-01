import Control.Monad (replicateM)
import Data.List (nub)
import System.Environment (getArgs)

{-
   Solver for Day 1 of the Advent of Code 2020.
   Problem description here: https://adventofcode.com/2020/day/1

   Takes a command line argument indicating which part to solve (Day 1 has two
   parts, 1 and 2), reads input from stdin, and prints solution to stdout.
-}

main :: IO ()
main = do
  part <- getArgs
  case part of
    ["1"]     -> solve 2
    ["2"]     -> solve 3
    otherwise -> putStrLn "Valid options are 1 or 2"

solve :: Int -> IO ()
solve n = do
  report <- map read . lines <$> getContents
  putStrLn $ show $ product $ findNums n report

findNums :: Int -> [Int] -> [Int]
findNums n = map snd . head . filter sumsTo2020 . replicateM n . zip [0..]

sumsTo2020 :: [(Int, Int)] -> Bool
sumsTo2020 xs = not (anySame (map fst xs)) && sum (map snd xs) == 2020

anySame :: Eq a => [a] -> Bool
anySame xs = length (nub xs) < length xs
