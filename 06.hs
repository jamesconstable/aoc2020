import Data.Foldable (foldl1)
import Data.Set (Set, fromList, intersection, size, union)

import Runner (runner)

{-|
   Solver for Day 6 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/6
-}

main :: IO ()
main = runner (solveWith union) (solveWith intersection)

solveWith :: (Set Char -> Set Char -> Set Char) -> String -> Int
solveWith fn = sum . map size . groupAnswers fn [] . lines

groupAnswers :: Ord a
             => (Set a -> Set a -> Set a) -> [Set a] -> [[a]] -> [Set a]
groupAnswers fn acc xs = case xs of
  []      -> [foldl1 fn acc]
  ([]:xs) -> foldl1 fn acc : groupAnswers fn [] xs
  (x:xs)  -> groupAnswers fn (fromList x : acc) xs
