import Data.Char (digitToInt, intToDigit, isDigit)
import Data.IntMap (IntMap, elems, empty, insert)
import Data.List (isPrefixOf, foldl')
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt, showIntAtBase)

import Runner (runner)

{-|
   Solver for Day 14 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/14
-}

data Command = Mask String | Write Int Int

data State = State {
  getMask :: String,
  getMem  :: IntMap Int
}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = execute (const pure) maskValue . parseInput

solve2 :: String -> Int
solve2 = execute maskExpand (const id) . parseInput

execute :: (String -> Int -> [Int]) -> (String -> Int -> Int) -> [Command] -> Int
execute addrFn valueFn =
  sum . elems . getMem . foldl' execute' (State (replicate 36 'X') empty)
  where
    execute' (State mask mem) command = case command of
      Mask mask' -> State mask' mem
      Write a v  -> State mask $ updateMem (addrFn mask a) (valueFn mask v) mem

updateMem :: [Int] -> Int -> IntMap Int -> IntMap Int
updateMem addrs value mem = foldl' (\m a -> insert a value m) mem addrs

maskValue :: String -> Int -> Int
maskValue mask = fromJust
  . readBinary
  . zipWith (\m v -> if isDigit m then m else v) mask
  . showBinary

maskExpand :: String -> Int -> [Int]
maskExpand mask = map (fromJust . readBinary) . expand mask . showBinary
  where
    expand m ~(v:vs) = case m of
      []     -> [[]]
      '0':ms -> (v:) <$> expand ms vs
      '1':ms -> ('1':) <$> expand ms vs
      _:ms   -> [('0':), ('1':)] <*> expand ms vs

showBinary :: Int -> String
showBinary v = padLeft '0' 36 $ showIntAtBase 2 intToDigit v ""

readBinary :: String -> Maybe Int
readBinary = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

padLeft :: Char -> Int -> String -> String
padLeft c n s = replicate (n - length s) c <> s

parseInput :: String -> [Command]
parseInput = map parse' . lines
  where
    parse' line
      | "mask" `isPrefixOf` line = Mask (words line !! 2)
      | "mem[" `isPrefixOf` line =
        Write (read $ takeWhile isDigit $ drop 4 line) (read $ words line !! 2)
