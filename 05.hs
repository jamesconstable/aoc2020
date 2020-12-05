import Data.List (sort)
import Data.Maybe (fromJust)
import Numeric (readInt)

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

findGap :: [Int] -> Maybe Int
findGap (x1:x2:xs) = if x2 - x1 > 1 then Just (x1+1) else findGap (x2:xs)
findGap _          = Nothing

parseSeatIds :: String -> [Int]
parseSeatIds = map toSeatId . lines

toSeatId :: String -> Int
toSeatId = fst . head . readInt 2 (`elem` "FBLR") (fromEnum . (`elem` "BR"))
