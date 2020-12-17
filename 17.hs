import Control.Monad (guard, replicateM)
import Data.Map.Strict (Map, elems, empty, insert, findWithDefault, fromList)
import Data.List (foldl')

import qualified Data.Ix as I

import Runner (runner)

{-|
   Solver for Day 17 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/17
-}

type Coord = [Int]

type Grid = Map Coord Char

data State = State {
  bounds :: (Coord, Coord),
  grid   :: Grid
}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = countActive . (!! 6) . iterate conwayStep . readState 3

solve2 :: String -> Int
solve2 = countActive . (!! 6) . iterate conwayStep . readState 4

countActive :: State -> Int
countActive = length . filter (== '#') . elems . grid

conwayStep :: State -> State
conwayStep (State b g) =
  let b' = incrementBounds b
  in State b' $ foldl' (\m i -> insert i (cellStep i g) m) empty (range b')

incrementBounds :: (Coord, Coord) -> (Coord, Coord)
incrementBounds (lowers, uppers) = (map (subtract 1) lowers, map (+1) uppers)

range :: (Coord, Coord) -> [Coord]
range = mapM I.range . uncurry zip

cellStep :: Coord -> Grid -> Char
cellStep i m =
  let neighbourSum = length $ filter (== '#') $ neighbours i m
  in if findWithDefault '.' i m == '#'
    then if neighbourSum == 2 || neighbourSum == 3 then '#' else '.'
    else if neighbourSum == 3 then '#' else '.'

neighbours :: Coord -> Grid -> [Char]
neighbours i m =
  map ((\c -> findWithDefault '.' c m) . zipWith (+) i) $ directions $ length i

directions :: Int -> [Coord]
directions = filter (not . all (== 0)) . (`replicateM` [-1, 0, 1])

padLeft :: Int -> a -> [a] -> [a]
padLeft width filler es = replicate (width - length es) filler <> es

readState :: Int -> String -> State
readState d input = State bounds grid
  where
    ls = lines input
    bounds = (replicate d 0, padLeft d 0 [length ls, length (head ls)])
    grid = fromList $ do
      (y, l) <- zip [0..] ls
      (x, c) <- zip [0..] l
      return (padLeft d 0 [y, x], c)
