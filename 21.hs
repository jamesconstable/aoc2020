import Data.List ((\\), foldl', intercalate, intersect, isPrefixOf, nub)
import Data.Map (Map, alter, elems, empty)
import Data.Maybe (fromMaybe)

import Runner (runner)

{-|
   Solver for Day 21 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/21
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 input =
  let
    labels               = parseInput input
    allergenCandidateMap = buildAllergenMap labels
    allIngredients       = concatMap fst labels
    possibleAllergens    = nub $ concat $ elems allergenCandidateMap
    safeIngredients      = nub allIngredients \\ possibleAllergens
  in length $ filter (`elem` safeIngredients) allIngredients

solve2 :: String -> String
solve2 = intercalate "," . eliminate . elems . buildAllergenMap . parseInput

buildAllergenMap :: [([String], [String])] -> Map String [String]
buildAllergenMap = foldl' insertAllergens empty
  where
    insertAllergens m (is, as) = foldl' (\m a -> intersectInsert a is m) m as
    intersectInsert k v = alter (Just . intersect v . fromMaybe v) k

eliminate :: Eq a => [[a]] -> [a]
eliminate = map head . fixpoint (eliminate' . zipper)
  where
    remove e = map (filter (/= e))
    eliminate' (t@(~(x:xs)), bs)
      | null t    = reverse bs
      | [x'] <- x = eliminate' (remove x' xs, x : remove x' bs)
      | otherwise = eliminate' (xs, x:bs)

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f i =
  let i' = f i
  in if i' == i then i' else fixpoint f i'

zipper :: [a] -> ([a], [a])
zipper xs = (xs, [])

parseInput :: String -> [([String], [String])]
parseInput input = parseLine <$> lines input

parseLine :: String -> ([String], [String])
parseLine l =
  let [ingredients, allergens] = splitOn " (contains " l
  in (words ingredients, splitOn ", " (init allergens))

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s = splitOn' . zipper
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t           = [reverse bs]
      | s `isPrefixOf` t = reverse bs : splitOn' (drop (length s) t, [])
      | otherwise        = splitOn' (xs, x:bs)
