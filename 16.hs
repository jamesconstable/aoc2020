import Data.List (isPrefixOf, foldl')

import Runner (runner)

{-|
   Solver for Day 16 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/16
-}

data Rule = Rule { label :: String, ranges :: [(Int, Int)] }
  deriving (Eq, Show)

type Ticket = [Int]

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let (rules, _, tickets) = parseInput input
  in sum $ concatMap (invalidFields rules) tickets

solve2 :: String -> Int
solve2 input =
  let
    (rules, ticket, tickets) = parseInput input
    validTickets = filter (null . invalidFields rules) tickets
    allRules = [rules | _ <- rules]
    orderedRules = eliminate $ filterCandidateRules allRules validTickets
    labelledTicket = zip (label <$> orderedRules) ticket
  in product $ snd <$> filter (isPrefixOf "departure" . fst) labelledTicket

invalidFields :: [Rule] -> Ticket -> [Int]
invalidFields rules = filter (not . validByAnyRule rules)

isValid :: Rule -> Int -> Bool
isValid (Rule _ ranges) v = any (\(start, end) -> start <= v && v <= end) ranges

validByAnyRule :: [Rule] -> Int -> Bool
validByAnyRule rules v = any (`isValid` v) rules

filterCandidateRules :: [[Rule]] -> [Ticket] -> [[Rule]]
filterCandidateRules = foldl' $ zipWith (\rs f -> filter (`isValid` f) rs)

eliminate :: Eq a => [[a]] -> [a]
eliminate = map head . fixpoint (eliminate' . zipperAt 0)
  where
    remove e = map (filter (/= e))
    eliminate' (t@(~(x:xs)), bs)
      | null t    = reverse bs
      | [x'] <- x = eliminate' (remove x' xs, x : remove x' bs)
      | otherwise = eliminate' (xs, x:bs)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f i =
  let i' = f i
  in if i' == i then i' else fixpoint f i'

zipperAt :: Int -> [a] -> ([a], [a])
zipperAt i xs = (drop i xs, reverse (take i xs))

parseInput :: String -> ([Rule], Ticket, [Ticket])
parseInput input =
  let
    [ruleLines, [_, ticketLine], _:ticketLines] = splitOn [""] $ lines input
    rules = map parseRule ruleLines
    ticket = map read $ splitOn "," ticketLine
    tickets = map (map read . splitOn ",") ticketLines
  in (rules, ticket, tickets)

parseRule :: String -> Rule
parseRule l =
  let
    [label, t] = splitOn ": " l
    rangeSpans = splitOn " or " t
    ranges = map ((\[s, e] -> (s, e)) . map read . splitOn "-") rangeSpans
  in Rule label ranges

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s = splitOn' . zipperAt 0
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t           = [reverse bs]
      | s `isPrefixOf` t = reverse bs : splitOn' (drop (length s) t, [])
      | otherwise        = splitOn' (xs, x:bs)
