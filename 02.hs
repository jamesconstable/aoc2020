import Runner (runner)

{-|
   Solver for Day 2 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/2
-}

data Password = Password Int Int Char String

main :: IO ()
main = runner (solve isValid1) (solve isValid2)

solve :: (Password -> Bool) -> [Char] -> Int
solve isValid input =
  let passwords = map parseLine (lines input)
  in length $ filter isValid passwords

isValid1 :: Password -> Bool
isValid1 (Password n1 n2 c p) =
  let count = length $ filter (== c) p
  in n1 <= count && count <= n2

isValid2 :: Password -> Bool
isValid2 (Password n1 n2 c p) =
  let
    condition1 = p !! (n1 - 1) == c
    condition2 = p !! (n2 - 1) == c
  in condition1 /= condition2

parseLine :: String -> Password
parseLine s =
  let
    (num1, '-':rest) = break (== '-') s
    [num2, letter:":", password] = words rest
  in Password (read num1) (read num2) letter password
