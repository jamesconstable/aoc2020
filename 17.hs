import Control.Monad (guard, replicateM)
import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Map.Strict (Map, (!), assocs, elems, empty, insert, keys, findWithDefault, fromList)
import Data.List (isPrefixOf, foldl')
import Data.Maybe (fromJust, listToMaybe)
import Numeric (readInt, showIntAtBase)

import Runner (runner)

{-|
   Solver for Day 17 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/17
-}

type Coord = [Int]
type Grid = Map Coord Char

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let
    grid = initGrid 3 input
    finalGrid = iterate (conwayStep 3) grid !! 6
  in length $ filter (== '#') $ elems finalGrid

solve2 :: String -> Int
solve2 input =
  let
    grid = initGrid 4 input
    finalGrid = iterate (conwayStep 4) grid !! 6
  in length $ filter (== '#') $ elems finalGrid

conwayStep :: Int -> Grid -> Grid
conwayStep d g = foldl' (\m i -> insert i (cellStep d i g) m) empty $
  allIndices (replicate (d-2) 1 <> [8, 8]) 6

cellStep :: Int -> Coord -> Grid -> Char
cellStep d i m =
  let neighbourSum = length $ filter (== '#') $ neighbours d i m
  in if findWithDefault '.' i m == '#'
    then if neighbourSum == 2 || neighbourSum == 3 then '#' else '.'
    else if neighbourSum == 3 then '#' else '.'

neighbours :: Int -> Coord -> Grid -> [Char]
neighbours d i m =  map ((\k -> findWithDefault '.' k m) . zipWith (+) i) (directions d)

allIndices :: Coord -> Int -> [Coord]
allIndices initialDimensions steps =
  mapM (\d -> [(-steps) .. (d + steps - 1)]) initialDimensions

directions :: Int -> [Coord]
directions d = filter (not . all (== 0)) $ replicateM d [-1, 0, 1]

initGrid :: Int -> String -> Grid
initGrid d input = fromList $ do
  (y, l) <- zip [0..] (lines input)
  (x, c) <- zip [0..] l
  return (replicate (d-2) 0 <> [y, x], c)
