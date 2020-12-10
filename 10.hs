import Data.List (sort, tails)
import Data.Maybe (fromJust)

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
solve2 = fromJust . countCompositions . joltageDiffs

joltageDiffs :: String -> [Int]
joltageDiffs input =
  let
    adapters = sort $ map read $ lines input
    joltages = [0] <> adapters <> [last adapters + 3]
  in zipWith (-) (tail joltages) joltages

countCompositions :: [Int] -> Maybe Int
countCompositions ds = case ds of
  []  -> Just 1
  1:_ -> let (ones, t) = break (/= 1) ds
         in (tribonacci !! sum ones *) <$> countCompositions t
  3:t -> countCompositions t
  _   -> Nothing    -- This solution fails if there are any 2s in the list, but
                    -- we know from Part 1 that this isn't the case.

tribonacci :: [Int]
tribonacci = 1 : 1 : 2 : map (sum . take 3) (tails tribonacci)
