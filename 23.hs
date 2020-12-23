import Data.Char (digitToInt, isDigit)
import Data.Function ((&))
import Data.IntMap.Strict (IntMap, (!), insert, fromList)

import Runner (runner)

{-|
   Solver for Day 23 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/23
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> String
solve1 input =
  let (_, cups) = iterate moveCups (initCircle 0 input) !! 100
  in concatMap show $ tail $ take (length cups) $ iterate (cups !) 1

solve2 :: String -> Int
solve2 input =
  let (_, cups) = iterate moveCups (initCircle 1000000 input) !! 10000000
  in (cups ! 1) * (cups ! (cups ! 1))

initCircle :: Int -> String -> (Int, IntMap Int)
initCircle n s =
  let
    ds = filter isDigit s
    ns = map digitToInt ds <> [length ds + 1 .. n]
  in (head ns, fromList $ zip ns (tail ns <> ns))

moveCups :: (Int, IntMap Int) -> (Int, IntMap Int)
moveCups (current, cups) =
  let
    _ : r1 : r2 : r3 : next : _ = iterate (cups !) current
    wrap i = if i <= 0 then i + length cups else i
    dest:_ = filter (`notElem` [r1, r2, r3]) $ map (wrap . (current -)) [1..]
    cups'  = cups
      & insert current next
      & insert dest r1
      & insert r3 (cups ! dest)
  in (next, cups')
