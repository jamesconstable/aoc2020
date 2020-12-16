import Data.Char (digitToInt, intToDigit, isDigit)
import Data.IntMap (IntMap, elems, empty, insert)
import Data.List (isPrefixOf, foldl')
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt, showIntAtBase)

import Runner (runner)

{-|
   Solver for Day 16 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/16
-}

data Rule = Rule {
  label :: String,
  r1    :: (Int, Int),
  r2    :: (Int, Int)
} deriving (Eq, Show)

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let (rules, tickets, _) = parseInput input
  in sum $ concatMap (invalidFields rules) tickets

solve2 :: String -> Int
solve2 input =
  let
    (rules, tickets, ticket) = parseInput input
    validTickets =
      map snd
      $ filter (null . fst)
      $ map (\t -> (invalidFields rules t, t)) tickets
    orderedRules = simplifyRules $ rulesReduce (map (const rules) rules) validTickets
    myTicket = zip orderedRules ticket
  in product
    $ map snd
    $ filter ((\(Rule l _ _) -> "departure" `isPrefixOf` l) . fst) myTicket

invalidFields :: [Rule] -> [Int] -> [Int]
invalidFields rules = filter (not . validByAnyRule rules)

validByAnyRule :: [Rule] -> Int -> Bool
validByAnyRule rules v =
  any (\(Rule _ r1 r2) -> inRange r1 v || inRange r2 v) rules

inRange :: (Int, Int) -> Int -> Bool
inRange (s, e) v = s <= v && v <= e

simplifyRules :: [[Rule]] -> [Rule]
simplifyRules = map head . fixpoint (\rs -> simplify' (rs, []))

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f i =
  let i' = f i
  in if i' == i then i' else fixpoint f i'

simplify' :: ([[Rule]], [[Rule]]) -> [[Rule]]
simplify' ([], bs) = reverse bs
simplify' (x:xs, bs) =
  if length x == 1
  then
    let
      xs' = map (filter (\r -> label r /= label (head x))) xs
      bs' = map (filter (\r -> label r /= label (head x))) bs
    in simplify' (xs', x:bs')
  else simplify' (xs, x:bs)

rulesReduce :: [[Rule]] -> [[Int]] -> [[Rule]]
rulesReduce = foldl' ruleReduce

ruleReduce :: [[Rule]] -> [Int] -> [[Rule]]
ruleReduce = zipWith (\rs f -> filter (`valueValidate` f) rs)

valueValidate :: Rule -> Int -> Bool
valueValidate (Rule _ r1 r2) v = inRange r1 v || inRange r2 v

parseInput :: String -> ([Rule], [[Int]], [Int])
parseInput input =
  let
    (rules, t:_:_:ts) = parseRules $ lines input
    ticket = parseTicket t
    tickets = map parseTicket ts
  in (rules, tickets, ticket)

parseRules :: [String] -> ([Rule], [String])
parseRules ls =
  let (ruleLines, "":_:rest) = break (== "") ls
  in (map parseRule ruleLines, rest)

parseRule :: String -> Rule
parseRule l =
  let
    (label, ':':' ':t) = break (== ':') l
    (rawRange00, '-':t') = span isDigit t
    (rawRange01, ' ':'o':'r':' ':t'') = span isDigit t'
    (rawRange10, '-':rawRange11) = span isDigit t''
  in Rule label (read rawRange00, read rawRange01) (read rawRange10, read rawRange11)

parseTicket :: String -> [Int]
parseTicket l = map read $ words $ map (\c -> if c == ',' then ' ' else c) l
