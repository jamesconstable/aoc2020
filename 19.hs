import Data.IntMap (IntMap, (!), insert, fromList)
import Data.Function ((&))
import Data.List (foldl', isPrefixOf)

import Runner (runner)

{-|
   Solver for Day 19 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/19
-}

data Rule = Terminal Char | Compound [[Int]]

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let (rules, messages) = parseInput input
  in length $ filter (matches 0 rules) messages

solve2 :: String -> Int
solve2 input =
  let
    (rules, messages) = parseInput input
    rules' = rules
      & insert 8 (Compound [[42], [42, 8]])
      & insert 11 (Compound [[42, 31], [42, 11, 31]])
  in length $ filter (matches 0 rules') messages

matches :: Int -> IntMap Rule -> String -> Bool
matches ruleNumber rules = any null . dropMatches ruleNumber
  where
    dropMatches ruleNumber s = case rules ! ruleNumber of
      Terminal c  -> if [c] `isPrefixOf` s then [tail s] else []
      Compound os -> concatMap (foldl' (flip (concatMap . dropMatches)) [s]) os

parseInput :: String -> (IntMap Rule, [String])
parseInput input =
  let [rules, messages] = splitOn [""] $ lines input
  in (fromList $ parseRule <$> rules, messages)

parseRule :: String -> (Int, Rule)
parseRule s =
  let
    [n, ruleBody] = splitOn ": " s
    ruleValue = if head ruleBody == '"'
      then Terminal $ ruleBody !! 1
      else Compound $ map read . words <$> splitOn " | " ruleBody
  in (read n, ruleValue)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s t = splitOn' (t, [])
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t           = [reverse bs]
      | s `isPrefixOf` t = reverse bs : splitOn' (drop (length s) t, [])
      | otherwise        = splitOn' (xs, x:bs)
