import Data.Array (Array, (!), array, bounds, elems, indices, listArray)
import Data.Ix (Ix, inRange)
import Data.Maybe (isJust, fromJust, listToMaybe)

import Runner (runner)

{-|
   Solver for Day 11 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/11
-}

type Seating = Array (Int, Int) Char

main :: IO ()
main = runner (solve False) (solve True)

solve :: Bool -> String -> Int
solve isPart2 = length . filter (== '#') . elems
  . fixpoint (updateSeating isPart2) . parseSeating

parseSeating :: String -> Seating
parseSeating input =
  let l = lines input
  in listArray ((0, 0), (length l - 1, length (head l) - 1)) $ concat l

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint fn i =
  let i' = fn i
  in if i' == i then i' else fixpoint fn i'

updateSeating :: Bool -> Seating -> Seating
updateSeating isPart2 s =
  array (bounds s) $ map (\i -> (i, updateSeat isPart2 i s)) $ indices s

(!?) :: Ix i => Array i e -> i -> Maybe e
m !? i = if inRange (bounds m) i then Just (m ! i) else Nothing

addTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addTuple (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

firstVisible :: Seating -> (Int, Int) -> (Int, Int) -> Maybe Char
firstVisible seating viewer direction = listToMaybe
  $ dropWhile (== '.')
  $ map fromJust
  $ takeWhile isJust
  $ map (seating !?)
  $ tail
  $ iterate (addTuple direction) viewer

updateSeat :: Bool -> (Int, Int) -> Seating -> Char
updateSeat isPart2 i s =
  update (s ! i) $ countNeighbours (if isPart2 then neighbour2 else neighbour1)
  where
    neighbour1 = (s !?) . addTuple i  -- Gets immediately adjacent neighbour
    neighbour2 = firstVisible s i     -- Gets first visible neighbour
    update current neighbours
      | current == '.'                 = '.'
      | neighbours == 0                = '#'
      | not isPart2 && neighbours >= 4 = 'L'
      | isPart2     && neighbours >= 5 = 'L'
      | otherwise                      = current

countNeighbours :: ((Int, Int) -> Maybe Char) -> Int
countNeighbours getNeighbour =
  let directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  in length $ filter (== Just '#') $ map getNeighbour directions
