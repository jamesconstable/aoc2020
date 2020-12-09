import Data.List (inits, tails)

import Runner (runner)

{-|
   Solver for Day 9 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/9
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = findInvalid 25 . map read . lines

solve2 :: String -> Int
solve2 input =
  let
    ns = map read $ lines input
    sumList = sublistWithSum (findInvalid 25 ns) ns
  in minimum sumList + maximum sumList

findInvalid :: Int -> [Int] -> Int
findInvalid windowSize = findInvalid' . zipperAt windowSize
  where
    findInvalid' (x:xs, bs) =
      if x `elem` pairSums (take windowSize bs)
      then findInvalid' (xs, x:bs)
      else x

zipperAt :: Int -> [a] -> ([a], [a])
zipperAt n xs = (drop n xs, reverse (take n xs))

pairSums :: [Int] -> [Int]
pairSums ns = [x + y | x:xs <- tails ns, y <- xs]

sublistWithSum :: Int -> [Int] -> [Int]
sublistWithSum v ns =
  let sumInits = zip (scanl (+) 0 ns) (inits ns)
  in case dropWhile ((< v) . fst) sumInits of
    (s, l):_ | s == v -> l
    _                 -> sublistWithSum v (tail ns)
