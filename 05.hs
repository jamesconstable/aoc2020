import Data.List (sort)
import Data.Maybe (fromJust)

import Runner (runner)

{-|
   Solver for Day 5 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/5
-}

main :: IO ()
main = runner maxId missingId

maxId :: String -> Int
maxId = maximum . parseSeatIds

missingId :: String -> Int
missingId = fromJust . findGap . sort . parseSeatIds

findGap :: (Enum a, Eq a) => [a] -> Maybe a
findGap (x1:x2:xs) = if succ x1 /= x2 then Just (succ x1) else findGap (x2:xs)
findGap _          = Nothing

parseSeatIds :: String -> [Int]
parseSeatIds = map toSeatId . lines

toSeatId :: Foldable f => f Char -> Int
toSeatId = foldl (\x y -> x*2 + fromEnum (y `elem` "BR")) 0
