import Data.List (foldl')
import Data.Maybe (catMaybes, mapMaybe)

import Runner (runner)

{-|
   Solver for Day 13 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/13
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Integer
solve1 input =
  let
    (time, buses) = parseInput input
    (wait, bus) = minimum $ map (\b -> (waitTime time b, b)) $ catMaybes buses
  in wait * bus

solve2 :: String -> Integer
solve2 input =
  let
    (_, buses) = parseInput input
    getOffset (i, bus) = bus >>= \b -> Just (b-i, b)
    busOffsets = mapMaybe getOffset $ zip [0..] buses
  in fst $ chineseRemainder busOffsets

waitTime :: Integral a => a -> a -> a
waitTime time bus = bus - time `mod` bus

chineseRemainder :: Integral a => [(a, a)] -> (a, a)
chineseRemainder = foldl' chineseRemainder' (0, 1)
  where
    chineseRemainder' (r1, m1) (r2, m2) =
      let
        r = r2 + m2 * (r1 - r2) * modularInverse m2 m1
        m = m2 * m1
      in (r `mod` m, m)

modularInverse :: Integral a => a -> a -> a
modularInverse x m =
  let (_, i, _) = gcdExtended x m
  in i `mod` m

gcdExtended :: Integral a => a -> a -> (a, a, a)
gcdExtended 0 b = (b, 0, 1)
gcdExtended a b =
  let (gcd, x, y) = gcdExtended (b `mod` a) a
  in (gcd, y - (b `div` a) * x, x)

parseInput :: String -> (Integer, [Maybe Integer])
parseInput input =
  let [l1, l2] = lines input
  in (read l1, parseBuses l2)

parseBuses :: String -> [Maybe Integer]
parseBuses cs = case cs of
  []      -> []
  (',':t) -> parseBuses t
  ('x':t) -> Nothing : parseBuses t
  _       -> let (n, t) = break (== ',') cs
             in Just (read n) : parseBuses t
