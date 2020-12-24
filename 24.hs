import qualified Data.Set as S

import Runner (runner)

{-|
   Solver for Day 24 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/24
-}

data HexDirections = E | SE | SW | W | NW | NE
  deriving (Bounded, Enum, Eq)

type HexPoint = (Int, Int)

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = S.size . readTiles

solve2 :: String -> Int
solve2 = S.size . (!! 100) . iterate updateTiles . readTiles

readTiles :: String -> S.Set HexPoint
readTiles = fromListWithXor . map (followDirections (0, 0) . parsePath) . lines

fromListWithXor :: Ord a => [a] -> S.Set a
fromListWithXor = foldl toggle S.empty
  where toggle es e = if e `S.member` es then S.delete e es else S.insert e es

updateTiles :: S.Set HexPoint -> S.Set HexPoint
updateTiles ts =
  let ts' = S.elems ts
  in S.fromList $ filter (`isBlackNext` ts) $ ts' <> concatMap neighbours ts'

isBlackNext :: HexPoint -> S.Set HexPoint -> Bool
isBlackNext t ts =
  let neighbourCount = length $ filter (`S.member` ts) (neighbours t)
  in if t `S.member` ts
    then neighbourCount == 1 || neighbourCount == 2
    else neighbourCount == 2

neighbours :: HexPoint -> [HexPoint]
neighbours i = map (followDirections i . (:[])) [minBound..]

followDirections :: HexPoint -> [HexDirections] -> HexPoint
followDirections (y, x) steps = case steps of
  []    -> (y, x)
  E:ds  -> followDirections (y, x+1) ds
  SE:ds -> followDirections (y+1, x + y `mod` 2) ds
  SW:ds -> followDirections (y+1, x + y `mod` 2 - 1) ds
  W:ds  -> followDirections (y, x-1) ds
  NW:ds -> followDirections (y-1, x + y `mod` 2 - 1) ds
  NE:ds -> followDirections (y-1, x + y `mod` 2) ds

parsePath :: String -> [HexDirections]
parsePath s = case s of
  []        -> []
  'e':t     -> E :parsePath t
  's':'e':t -> SE:parsePath t
  's':'w':t -> SW:parsePath t
  'w':t     -> W :parsePath t
  'n':'w':t -> NW:parsePath t
  'n':'e':t -> NE:parsePath t
