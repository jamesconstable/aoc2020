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
solve1 = scoreGame . playCombat . readDecks

solve2 :: String -> Int
solve2 = scoreGame . playRecursiveCombat . readDecks

scoreGame :: (Int, Decks) -> Int
scoreGame (winner, decks) = sum $ zipWith (*) [1..] $ reverse (decks !! winner)

cardsToWinner :: Int -> Decks -> Decks
cardsToWinner winner [x:xs, y:ys] =
  if winner == 0
  then [xs ++ [x,y], ys]
  else [xs, ys ++ [y,x]]

playCombat :: Decks -> (Int, Decks)
playCombat decks = until (any null . snd) (playRound . snd) (-1, decks)
  where
    playRound :: Decks -> (Int, Decks)
    playRound ds@[x:_, y:_] =
      let w = if x > y then 0 else 1
      in (w, cardsToWinner w ds)

playRecursiveCombat :: Decks -> (Int, Decks)
playRecursiveCombat decks = playGame decks empty
  where
    playGame :: Decks -> Set Decks -> (Int, Decks)
    playGame decks seen =
      let (earlyExit, winner, decks') = playRound decks seen
      in if earlyExit || any null decks'
        then (winner, decks')
        else playGame decks' (insert decks seen)

    playRound :: Decks -> Set Decks -> (Bool, Int, Decks)
    playRound ds@[x:xs, y:ys] seen
      | member ds seen = (True, 0, ds)
      | x <= length xs && y <= length ys =
          let (w, _) = playGame [take x xs, take y ys] empty
          in (False, w, cardsToWinner w ds)
      | otherwise =
          let w = if x > y then 0 else 1
          in (False, w, cardsToWinner w ds)

readDecks :: String -> Decks
readDecks input =
  let (_:p1, _:_:p2) = break (== "") (lines input)
  in [map read p1, map read p2]
