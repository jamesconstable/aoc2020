import Data.Maybe (fromJust)
import Data.Map ((!))

import qualified Data.Map as M

import Runner (runner)

{-|
   Solver for Day 8 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/8
-}

data Op = Nop | Acc Int | Jmp Int

main :: IO ()
main = runner solve solve

solve :: String -> Int
solve input =
  let program = M.fromList $ zip [0..] $ zip (parseInput input) (repeat False)
  in runToLoop program 0 0

runToLoop :: M.Map Int (Op, Bool) -> Int -> Int -> Int
runToLoop program line acc =
  let continue (op, _) = runToLoop (M.insert line (op, True) program)
  in case program ! line of
    (_, True)    -> acc
    s@(Nop,   _) -> continue s (line + 1) acc
    s@(Acc x, _) -> continue s (line + 1) (acc + x)
    s@(Jmp x, _) -> continue s (line + x) acc

parseInput :: String -> [Op]
parseInput = map parseLine . lines
  where
    signToInt s = if s == '+' then 1 else -1
    parseLine l = case words l of
      ["nop", _]   -> Nop
      ["acc", s:n] -> Acc $ signToInt s * read n
      ["jmp", s:n] -> Jmp $ signToInt s * read n
