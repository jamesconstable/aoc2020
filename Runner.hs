module Runner (runner) where

import System.Environment (getArgs)

{-|
   Provides a common interface for running solvers.
   Expects a single command line argument indicating which part to run (1 or 2),
   reads input from stdin, and writes result to stdout.
-}
runner :: (Show a, Show b) => (String -> a) -> (String -> b) -> IO ()
runner part1 part2 = do
  let run fn = getContents >>= print . fn
  part <- getArgs
  case part of
    ["1"] -> run part1
    ["2"] -> run part2
    _     -> putStrLn "Valid options are 1 or 2"
