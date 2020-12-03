import System.Environment (getArgs)

{-
   Solver for Day 3 of the Advent of Code 2020
   Problem description here: https://adventofcode.com/2020/day/3

   Takes a command line argument indicating which part to solve (1 or 2), reads
   input from stdin, and prints solution to stdout.
-}

main :: IO ()
main = do
  part <- getArgs
  case part of
    ["1"]     -> solve [(3, 1)]
    ["2"]     -> solve [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    otherwise -> putStrLn "Valid options are 1 or 2"

solve :: [(Int, Int)] -> IO ()
solve slopes = do
  mapData <- lines <$> getContents
  putStrLn $ show $ product $ map (countCollisions mapData) slopes

countCollisions :: [[Char]] -> (Int, Int) -> Int
countCollisions mapData (sx, sy) = countCollisions' 0 0 0
  where
    width = length (head mapData)
    countCollisions' x y acc =
      let
        (x', y') = (x + sx, y + sy)
        collision = (mapData !! y') !! (x' `mod` width) == '#'
      in if y' >= length mapData
        then acc
        else countCollisions' x' y' (acc + if collision then 1 else 0)
