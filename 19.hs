import Data.IntMap (IntMap, (!), insert, fromList)
import Data.List (foldl', isPrefixOf)

import Runner (runner)

{-|
   Solver for Day 19 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/19
-}

type Rule = Either Char [[Int]]

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
    rules' =
      insert 11 (Right [[42, 31], [42, 11, 31]])
      $ insert 8 (Right [[42], [42, 8]]) rules
  in length $ filter (matches 0 rules') messages

matches :: Int -> IntMap Rule -> String -> Bool
matches ruleNumber rules = any null . dropMatches ruleNumber
  where
    dropMatches ruleNumber s = case rules ! ruleNumber of
      Left c   -> if [c] `isPrefixOf` s then [tail s] else []
      Right os -> concatMap (foldl' (flip (concatMap . dropMatches)) [s]) os

parseInput :: String -> (IntMap Rule, [String])
parseInput input =
  let [rules, messages] = splitOn [""] $ lines input
  in (fromList $ parseRule <$> rules, messages)

parseRule :: String -> (Int, Rule)
parseRule s =
  let
    [n, ruleBody] = splitOn ": " s
    ruleValue = if head ruleBody == '"'
      then Left $ ruleBody !! 1
      else Right $ map read . words <$> splitOn " | " ruleBody
  in (read n, ruleValue)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s t = splitOn' (t, [])
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t           = [reverse bs]
      | s `isPrefixOf` t = reverse bs : splitOn' (drop (length s) t, [])
      | otherwise        = splitOn' (xs, x:bs)
