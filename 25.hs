import Data.List (elemIndex)

import Runner (runner)

{-|
   Solver for Day 25 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/25
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let
    [card, door] = read <$> lines input
    Just c = elemIndex card (transforms 7)
  in transforms door !! c

solve2 :: String -> String
solve2 _ = "The End, no Part 2 :)"

transforms :: Int -> [Int]
transforms subj = iterate (\v -> v * subj `mod` 20201227) 1
