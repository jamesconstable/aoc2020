import Data.List (foldl')

import qualified Data.Map as M
import qualified Data.Set as S

import Runner (runner)

{-|
   Solver for Day 7 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/7
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let
    assocs = do
      (outer, inners) <- parseLine <$> lines input
      (inner, _)      <- inners
      return (inner, outer)
    prependValue m (k, v) = M.insertWith (++) k [v] m
    rules = foldl' prependValue M.empty assocs
  in length $ reachableFrom "shiny gold" rules

solve2 :: String -> Int
solve2 = countContents "shiny gold" . M.fromList . map parseLine . lines

reachableFrom :: String -> M.Map String [String] -> S.Set String
reachableFrom k m =
  let vs = M.findWithDefault [] k m
  in S.unions (S.fromList vs : map (`reachableFrom` m) vs)

countContents :: String -> M.Map String [(String, Int)] -> Int
countContents k m =
  sum $ map (\(c, n) -> n * (1 + countContents c m)) $ M.findWithDefault [] k m

parseLine :: String -> (String, [(String, Int)])
parseLine s =
  let (c1 : c2 : _ : _ : t) = words s
  in (unwords [c1, c2], parseContents t)

parseContents :: [String] -> [(String, Int)]
parseContents x = case x of
  (n : c1 : c2 : _ : t) -> (unwords [c1, c2], read n) : parseContents t
  _                     -> []
