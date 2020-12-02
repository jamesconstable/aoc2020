import System.Environment (getArgs)

{-
   Solver for Day 2 of the Advent of Code 2020.
   Problem description here: https://adventofcode.com/2020/day/2

   Takes a command line argument indicating which part to solve (1 or 2), reads
   input from stdin, and prints solution to stdout.
-}

data Password = Password Int Int Char String

main :: IO ()
main = do
  part <- getArgs
  case part of
    ["1"]     -> solve isValid1
    ["2"]     -> solve isValid2
    otherwise -> putStrLn "Valid options are 1 or 2"

solve :: (Password -> Bool) -> IO ()
solve isValid = do
  passwords <- map parseLine . lines <$> getContents
  let valid = filter isValid passwords
  putStrLn $ show $ length valid

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
