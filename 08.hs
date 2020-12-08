import Data.Maybe (fromJust)
import qualified Data.Map as M

import Runner (runner)

{-|
   Solver for Day 8 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/8
-}

data Op = Nop Int | Acc Int | Jmp Int | End

data LoopBehaviour = Error | Return deriving Eq

type Program = M.Map Int Op

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = fromJust . run Return . parseProgram

solve2 :: String -> Int
solve2 = fromJust . head . filter (/= Nothing) . map (run Error) .
  generatePossibleFixes . parseProgram

run :: LoopBehaviour -> Program -> Maybe Int
run loop program = run' (M.fromList $ zip (M.keys program) (repeat False)) 0 0
  where
    run' visited line acc =
      let
        continue = run' (M.insert line True visited)
        inLoop = M.findWithDefault False line visited
      in case (inLoop, M.lookup line program) of
        (True, _)         -> if loop == Return then Just acc else Nothing
        (_, Nothing)      -> Nothing       -- Invalid line number
        (_, Just End)     -> Just acc      -- Successful exit
        (_, Just (Nop _)) -> continue (line + 1) acc
        (_, Just (Acc x)) -> continue (line + 1) (acc + x)
        (_, Just (Jmp x)) -> continue (line + x) acc

generatePossibleFixes :: Program -> [Program]
generatePossibleFixes program = generate' $ M.assocs program
  where
    generate' p = case p of
      []            -> []
      (n, Nop x):ls -> M.insert n (Jmp x) program : generate' ls
      (n, Jmp x):ls -> M.insert n (Nop x) program : generate' ls
      _:ls          -> generate' ls

parseProgram :: String -> Program
parseProgram = M.fromList . zip [0..] . (++ [End]) . map parseLine . lines
  where
    parseInt (s:n) = (if s == '+' then 1 else -1) * read n
    parseLine l = case words l of
      ["nop", v] -> Nop $ parseInt v
      ["acc", v] -> Acc $ parseInt v
      ["jmp", v] -> Jmp $ parseInt v
