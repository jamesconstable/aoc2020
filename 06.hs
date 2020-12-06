import Data.List (foldl1')
import Data.Set (Set, fromList, intersection, union)

import Runner (runner)

{-|
   Solver for Day 6 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/6
-}

main :: IO ()
main = runner (solveWith union) (solveWith intersection)

solveWith :: (Set Char -> Set Char -> Set Char) -> String -> Int
solveWith fn = sum . map length . groupAnswers fn [] . lines
  where
    groupAnswers fn acc []      = [foldl1' fn acc]
    groupAnswers fn acc ([]:xs) = foldl1' fn acc : groupAnswers fn [] xs
    groupAnswers fn acc (x:xs)  = groupAnswers fn (fromList x : acc) xs
