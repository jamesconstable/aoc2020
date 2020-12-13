import Data.List (sort, tails)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)

import Runner (runner)

{-|
   Solver for Day 13 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/13
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let
    (time, buses) = parseInput input
    (wait, bus) = minimum $ map (\x -> (minutesWait time x, x)) $ catMaybes buses
  in wait * bus

solve2 :: String -> Integer
solve2 input =
  let
    (_, buses) = parseInput input
    buses' = map (fmap toInteger) buses
    getRemainder (i, bus) = bus >>= \b -> Just (b-i, b)
    busOffsets = mapMaybe getRemainder (zip [0..] buses')
  in fst $ chineseRemainder busOffsets

minutesWait :: Integral a => a -> a -> a
minutesWait time bus =
  let lastBus = time `div` bus
  in (lastBus + 1) * bus - time

chineseRemainder :: Integral a => [(a, a)] -> (a, a)
chineseRemainder = foldr chineseRemainder' (0, 1)
  where
    chineseRemainder' (r1, m1) (r2, m2) =
      let
        r = r2 + m2 * (r1 - r2) * modularInverse m2 m1
        m = m2 * m1
      in (r `mod` m, m)

modularInverse :: Integral a => a -> a -> a
modularInverse x m =
  let (_, i, _) = extendedEuclidean x m
  in i `mod` m

extendedEuclidean :: Integral a => a -> a -> (a, a, a)
extendedEuclidean 0 b = (b, 0, 1)
extendedEuclidean a b =
  let (gcd, x, y) = extendedEuclidean (b `mod` a) a
  in (gcd, y - (b `div` a) * x, x)

parseInput :: String -> (Int, [Maybe Int])
parseInput input =
  let [l1, l2] = lines input
  in (read l1, parseBuses l2)

parseBuses :: String -> [Maybe Int]
parseBuses [] = []
parseBuses (',':t) = parseBuses t
parseBuses ('x':t) = Nothing : parseBuses t
parseBuses xs =
  Just (read (takeWhile (/= ',') xs)) : parseBuses (dropWhile (/= ',') xs)
