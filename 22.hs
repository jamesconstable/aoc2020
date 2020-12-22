import Data.Char (digitToInt, intToDigit, isDigit)
import Data.List (findIndex)
import Data.Maybe (fromJust, listToMaybe)
import Data.Set (Set, empty, insert, member)

import Runner (runner)

{-|
   Solver for Day 22 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/22
-}

type Decks = [[Int]]

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let (winner, decks) = playCombat (readDecks input)
  in scoreHand (decks !! winner)

solve2 :: String -> Int
solve2 input =
  let (winner, decks) = playRecursiveCombat (readDecks input)
  in scoreHand (decks !! winner)

playCombat :: Decks -> (Int, Decks)
playCombat decks =
  let decks' = head $ dropWhile (not . endGame) $ iterate playRound decks
  in (fromJust (findIndex (not . null) decks'), decks')

endGame :: Decks -> Bool
endGame = any null

scoreHand :: [Int] -> Int
scoreHand = sum . zipWith (*) [1..] . reverse

playRound :: Decks -> Decks
playRound ds@[x:xs, y:ys]
  | any null ds = ds
  | x > y       = [xs <> [x, y], ys]
  | otherwise   = [xs, ys <> [y, x]]

playRecursiveCombat :: Decks -> (Int, Decks)
playRecursiveCombat decks = (winner, decks')
  where
    (_, winner, decks') = playGame decks empty

    playGame :: Decks -> Set Decks -> (Bool, Int, Decks)
    playGame decks seen =
      let r@(wasEarlyExit, winner, decks') = playRound decks seen
      in if wasEarlyExit || endGame decks'
        then r
        else playGame decks' (insert decks seen)

    playRound :: Decks -> Set Decks -> (Bool, Int, Decks)
    playRound ds@[x:xs, y:ys] seen
      | member ds seen = (True, 0, ds)
      | x <= length xs && y <= length ys =
          let (r, w, [xs', ys']) = playGame [take x xs, take y ys] empty
          in if w == 0
            then (False, 0, [xs ++ [x,y], ys])
            else (False, 1, [xs, ys ++ [y,x]])
      | x > y     = (False, 0, [xs ++ [x,y], ys])
      | otherwise = (False, 1, [xs, ys ++ [y,x]])

readDecks :: String -> Decks
readDecks input =
  let (_:p1, _:_:p2) = break (== "") (lines input)
  in [map read p1, map read p2]
