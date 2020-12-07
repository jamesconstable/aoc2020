import Data.Foldable (fold)
import Data.List (concatMap, groupBy, nub, sort)
import Data.Map (Map, (!?), fromList)

import Runner (runner)

{-|
   Solver for Day 7 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/7
-}

main :: IO ()
main = runner solve solve

solve :: String -> Int
solve input =
  let
    rules = parseLine <$> lines input
    containeeAssocs = do
      (container, containees) <- rules
      (containee, _) <- containees
      return (containee, container)
    grouped = map (\kvs -> (fst (head kvs), map snd kvs))
      $ groupBy (\x y -> fst x == fst y) $ sort containeeAssocs
    reverseRulesMap = fromList grouped
  in length $ reachableFrom "shiny gold" reverseRulesMap

reachableFrom :: String -> Map String [String] -> [String]
reachableFrom s m = nub $ reachableFrom' s
  where
    reachableFrom' k =
      let vs = fold (m !? k)
      in vs ++ concatMap reachableFrom' vs

parseLine :: String -> (String, [(String, Int)])
parseLine s =
  let (c1 : c2 : _ : _ : t) = words s
  in (unwords [c1, c2], parseContents t)

parseContents :: [String] -> [(String, Int)]
parseContents x = case x of
  (n : c1 : c2 : _ : t) -> (unwords [c1, c2], read n) : parseContents t
  _                     -> []
