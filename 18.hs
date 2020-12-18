import Data.Map (Map, (!), fromList)

import Runner (runner)

{-|
   Solver for Day 18 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/18
-}

data Expr = Value Int | Add Expr Expr | Multiply Expr Expr

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = solveWithPrecedences $ fromList [("+", 0), ("*", 0)]

solve2 :: String -> Int
solve2 = solveWithPrecedences $ fromList [("+", 1), ("*", 0)]

solveWithPrecedences :: Map String Int -> String -> Int
solveWithPrecedences ps = sum . map (evaluate . parse ps . tokens) . lines

tokens :: String -> [String]
tokens = tokens' . lex
  where tokens' [(t, rest)] = if null t then [] else t : tokens' (lex rest)

parse :: Map String Int -> [String] -> Expr
parse precedences tokens = fst $ parse' tokens ([], [])
  where
    parse' tokens stacks@(args, ops) = case tokens of
      []     -> (stackFold stacks, [])
      "+":ts -> parse' ts $ stackPush (precedences ! "+", Add) stacks
      "*":ts -> parse' ts $ stackPush (precedences ! "*", Multiply) stacks
      "(":ts -> let (e, ts') = parse' ts ([], [])
                in parse' ts' (e:args, ops)
      ")":ts -> (stackFold stacks, ts)
      n:ts   -> parse' ts (Value (read n) : args, ops)

    stackPush new@(pNew, _) stacks@(args, ops@(~((pTop, _):_))) =
      if not (null ops) && pNew <= pTop
      then stackPush new (stackApply stacks)
      else (args, new:ops)

    stackApply (a2:a1:as, (_, o):os) = (o a1 a2 : as, os)

    stackFold = head . fst . until (null . snd) stackApply

evaluate :: Expr -> Int
evaluate expr = case expr of
  Value    n     -> n
  Add      e1 e2 -> evaluate e1 + evaluate e2
  Multiply e1 e2 -> evaluate e1 * evaluate e2
