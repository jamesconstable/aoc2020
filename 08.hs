import Data.Map (Map, (!?), assocs, fromList, insert)
import Data.Maybe (fromJust)

import qualified Data.Set as S

import Runner (runner)

{-|
   Solver for Day 8 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/8
-}

data Op = Nop Int | Acc Int | Jmp Int | End

data LoopBehaviour = Error | Return deriving Eq

type Program = Map Int Op

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = fromJust . run Return . parseProgram

solve2 :: String -> Int
solve2 = fromJust . head . filter (/= Nothing) . map (run Error) .
  generatePossibleFixes . parseProgram

run :: LoopBehaviour -> Program -> Maybe Int
run loop program = run' S.empty 0 0
  where
    run' visited line acc =
      let continue = run' (S.insert line visited)
      in case (S.member line visited, program !? line) of
        (True, _)         -> if loop == Return then Just acc else Nothing
        (_, Nothing)      -> Nothing       -- Invalid line number
        (_, Just End)     -> Just acc      -- Successful exit
        (_, Just (Nop _)) -> continue (line + 1) acc
        (_, Just (Acc x)) -> continue (line + 1) (acc + x)
        (_, Just (Jmp x)) -> continue (line + x) acc

generatePossibleFixes :: Program -> [Program]
generatePossibleFixes program = generate' $ assocs program
  where
    generate' p = case p of
      []            -> []
      (n, Nop x):ls -> insert n (Jmp x) program : generate' ls
      (n, Jmp x):ls -> insert n (Nop x) program : generate' ls
      _:ls          -> generate' ls

parseProgram :: String -> Program
parseProgram = fromList . zip [0..] . (++ [End]) . map parseLine . lines
  where
    parseInt (s:n) = (if s == '+' then 1 else -1) * read n
    parseLine l = case words l of
      ["nop", v] -> Nop $ parseInt v
      ["acc", v] -> Acc $ parseInt v
      ["jmp", v] -> Jmp $ parseInt v
