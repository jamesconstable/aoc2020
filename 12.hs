import Data.List (foldl')

import Runner (runner)

{-|
   Solver for Day 12 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/12
-}

type Vec2 = (Int, Int)

data Action = Action Char Int

data Ship a = Ship a Vec2

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = distance . foldl' moveDirect (Ship 0 (0, 0)) . parseInput

solve2 :: String -> Int
solve2 = distance . foldl' moveByWaypoint (Ship (10, 1) (0, 0)) . parseInput

moveDirect :: Ship Int -> Action -> Ship Int
moveDirect (Ship b p@(e, n)) (Action c v) = case c of
  'N' -> Ship b (e, n+v)
  'S' -> Ship b (e, n-v)
  'E' -> Ship b (e+v, n)
  'W' -> Ship b (e-v, n)
  'L' -> Ship (b+v) p
  'R' -> Ship (b-v) p
  'F' -> Ship b $ addVec p $ rotate b (0, v)

moveByWaypoint :: Ship Vec2 -> Action -> Ship Vec2
moveByWaypoint (Ship w@(we, wn) p@(e, n)) (Action c v) = case c of
  'N' -> Ship (we, wn+v) p
  'S' -> Ship (we, wn-v) p
  'E' -> Ship (we+v, wn) p
  'W' -> Ship (we-v, wn) p
  'L' -> Ship (rotate v w) p
  'R' -> Ship (rotate (-v) w) p
  'F' -> Ship w $ addVec p (v*we, v*wn)

rotate :: Int -> Vec2 -> Vec2
rotate degrees (x, y) =
  let
    r = radians $ fromIntegral degrees
    (x', y') = (fromIntegral x, fromIntegral y)
  in (round (x' * cos r - y' * sin r), round (x' * sin r + y' * cos r))

radians :: Float -> Float
radians d = d * pi / 180

addVec :: Vec2 -> Vec2 -> Vec2
addVec (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

distance :: Ship a -> Int
distance (Ship _ (x, y)) = abs x + abs y

parseInput :: String -> [Action]
parseInput = map (\(c:n) -> Action c (read n)) . lines
