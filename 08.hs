import Data.Maybe (fromJust)
import Data.Map ((!), (!?))

import qualified Data.Map as M

import Runner (runner)

{-|
   Solver for Day 8 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/8
-}

data Op = Nop Int | Acc Int | Jmp Int | End

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let program = M.fromList $ zip [0..] $ zip (parseInput input) (repeat False)
  in fromJust $ run program True 0 0

solve2 :: String -> Int
solve2 input =
  let
    program = M.fromList $ zip [0..] $ zip (parseInput input) (repeat False)
    possibleFixes = generatePossibleFixes program
    outcomes = map (\p -> run p False 0 0) possibleFixes
  in fromJust $ head $ filter (/= Nothing) outcomes

run :: M.Map Int (Op, Bool) -> Bool -> Int -> Int -> Maybe Int
run program exitOnLoop line acc =
  let
    continue (op, _) = run (M.insert line (op, True) program) exitOnLoop
    onLoop = if exitOnLoop then Just acc else Nothing
  in case program !? line of
    Nothing           -> Nothing    -- Invalid line number
    Just (_, True)    -> onLoop     -- Entering loop
    Just (End, _)     -> Just acc   -- Successful exit
    Just s@(Nop x, _) -> continue s (line + 1) acc
    Just s@(Acc x, _) -> continue s (line + 1) (acc + x)
    Just s@(Jmp x, _) -> continue s (line + x) acc

generatePossibleFixes :: M.Map Int (Op, a) -> [M.Map Int (Op, a)]
generatePossibleFixes program = generate' $ M.assocs program
  where
    generate' p = case p of
      []                 -> []
      (n, (Nop x, s)):ls -> M.insert n (Jmp x, s) program : generate' ls
      (n, (Jmp x, s)):ls -> M.insert n (Nop x, s) program : generate' ls
      _:ls               -> generate' ls

parseInput :: String -> [Op]
parseInput = (++ [End]) . map parseLine . lines
  where
    parseInt (s:n) = (if s == '+' then 1 else -1) * read n
    parseLine l = case words l of
      ["nop", v] -> Nop $ parseInt v
      ["acc", v] -> Acc $ parseInt v
      ["jmp", v] -> Jmp $ parseInt v
