import Data.Array ((!))
import Data.Function (on)
import Data.IntMap ((!?))
import Data.List (find, groupBy, isPrefixOf, foldl', sort, sortOn)
import Data.Maybe (fromJust, fromMaybe, isJust)

import qualified Data.Array as A
import qualified Data.IntMap as IM

import Runner (runner)

{-|
   Solver for Day 20 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/20
-}

data Side = R | T | L | B
  deriving (Eq, Ord)

data Tile = Tile {
  getId    :: Int,
  getArray :: A.Array (Int, Int) Char
}

data Edge = Edge {
  getCanonical :: Int,
  getRaw       :: Int,
  getTileId    :: Int,
  getSide      :: Side
}

main :: IO ()
main = runner solve1 solve2

solve1 :: String -> Int
solve1 = product . getCornerIds . getEdges . parseInput

solve2 :: String -> Int
solve2 input =
  let
    tiles = parseInput input
    edges = getEdges tiles
    topLeft = head (getCornerIds edges)
    edgeToEdges = edgeToEdgeMap edges
    tileToEdges = tileToEdgeMap (dropInnerEdges edges)
    fullPuzzle = connectTiles
      $ buildDown edgeToEdges tiles
      $ buildRight edgeToEdges tiles
      $ alignFirstCorner (tiles IM.! topLeft) (tileToEdges IM.! topLeft)
    n = head $ filter (/= 0) $ map countMonsters $ orientations fullPuzzle
  in countHashes fullPuzzle - n * countHashes seaMonster

edgeToEdgeMap :: [Edge] -> IM.IntMap [Edge]
edgeToEdgeMap = foldl' (\m e -> insertListMap (getCanonical e) e m) IM.empty

tileToEdgeMap :: [Edge] -> IM.IntMap [Edge]
tileToEdgeMap = foldl' (\m e -> insertListMap (getTileId e) e m) IM.empty

insertListMap :: Int -> a -> IM.IntMap [a] -> IM.IntMap [a]
insertListMap k v = IM.alter (Just . (v:) . fromMaybe []) k

countHashes :: A.Array i Char -> Int
countHashes = length . filter (== '#') . A.elems

seaMonster :: A.Array (Int, Int) Char
seaMonster = A.array ((0, 0), (length img - 1, length (head img) - 1)) $ do
    (y, row) <- zip [0..] img
    (x, ch)  <- zip [0..] row
    return ((y, x), ch)
  where img = ["                  # ",
               "#    ##    ##    ###",
               " #  #  #  #  #  #   "]

monsterAt :: (Int, Int) -> A.Array (Int, Int) Char -> Bool
monsterAt (y, x) img =
  let
    (_, (monsterYMax, monsterXMax)) = A.bounds seaMonster
    (_, (imgYMax, imgXMax)) = A.bounds img
    matchMask (j, i) = img ! (y+j, x+i) == '#' || seaMonster ! (j, i) /= '#'
  in monsterYMax + y <= imgYMax
    && monsterXMax + x <= imgXMax
    && all matchMask (A.indices seaMonster)

countMonsters :: A.Array (Int, Int) Char -> Int
countMonsters img = length $ filter (flip monsterAt img) $ A.indices img

connectTiles :: [[Tile]] -> A.Array (Int, Int) Char
connectTiles pieces =
  let
    (_, (tileMax, _)) = A.bounds $ getArray $ head $ head pieces
    tileSize = tileMax - 1
    puzzleSize = tileSize * length pieces - 1
  in A.array ((0, 0), (puzzleSize, puzzleSize)) $ do
    (y, row)          <- zip [0..] pieces
    (x, Tile _ piece) <- zip [0..] row
    (newJ, oldJ)      <- zip [0..] [1..tileMax-1]
    (newI, oldI)      <- zip [0..] [1..tileMax-1]
    return ((y * tileSize + newJ, x * tileSize + newI), piece ! (oldJ, oldI))

alignFirstCorner :: Tile -> [Edge] -> Tile
alignFirstCorner (Tile tileId tileArray) outerEdges =
  Tile tileId $ case sort (map getSide outerEdges) of
    [R, T] -> rotateArray $ rotateArray $ rotateArray tileArray
    [R, B] -> rotateArray $ rotateArray tileArray
    [L, B] -> rotateArray tileArray
    [T, L] -> tileArray

buildRight :: IM.IntMap [Edge] -> IM.IntMap Tile -> Tile -> [Tile]
buildRight edgeMap tileMap =
  map fromJust . takeWhile isJust . iterate nextTile . Just
  where nextTile = (>>= getNextTile rightEdge leftEdge edgeMap tileMap)

buildDown :: IM.IntMap [Edge] -> IM.IntMap Tile -> [Tile] -> [[Tile]]
buildDown edgeMap tileMap =
  map fromJust . takeWhile isJust . iterate nextRow . Just
  where nextRow = (>>= mapM (getNextTile bottomEdge topEdge edgeMap tileMap))

getNextTile :: (Tile -> [Char]) -> (Tile -> [Char]) -> IM.IntMap [Edge]
  -> IM.IntMap Tile -> Tile -> Maybe Tile
getNextTile getLastEdge getNextEdge edgeMap tileMap tile = do
  let lastEdge   = getLastEdge tile
  matchingEdges <- edgeMap !? canonicalId lastEdge
  nextEdge      <- find ((/= getId tile) . getTileId) matchingEdges
  nextTile      <- tileMap !? getTileId nextEdge
  find ((== lastEdge) . getNextEdge) (tileOrientations nextTile)

dropInnerEdges :: [Edge] -> [Edge]
dropInnerEdges =
   map head
  . filter ((== 1) . length)
  . groupBy ((==) `on` getCanonical)
  . sortOn getCanonical

getCornerIds :: [Edge] -> [Int]
getCornerIds =
  map (getTileId . head)
  . filter ((== 2) . length)
  . groupBy ((==) `on` getTileId)
  . sortOn getTileId
  . dropInnerEdges

topEdge :: Tile -> [Char]
topEdge t@(Tile _ tileArray) =
  let (_, (_, xMax)) = A.bounds tileArray
  in getRange ((0, 0), (0, xMax)) t

bottomEdge :: Tile -> [Char]
bottomEdge t@(Tile _ tileArray) =
  let (_, (yMax, xMax)) = A.bounds tileArray
  in getRange ((yMax, 0), (yMax, xMax)) t

leftEdge :: Tile -> [Char]
leftEdge t@(Tile _ tileArray) =
  let (_, (yMax, _)) = A.bounds tileArray
  in getRange ((0, 0), (yMax, 0)) t

rightEdge :: Tile -> [Char]
rightEdge t@(Tile _ tileArray) =
  let (_, (yMax, xMax)) = A.bounds tileArray
  in getRange ((0, xMax), (yMax, xMax)) t

getEdges :: IM.IntMap Tile -> [Edge]
getEdges tiles =
  [get rightEdge R, get topEdge T, get leftEdge L, get bottomEdge B]
    <*> IM.assocs tiles
  where get fn s (i, d) = Edge (canonicalId $ fn d) (readBinary $ fn d) i s

orientations :: A.Array (Int, Int) a -> [A.Array (Int, Int) a]
orientations array =
  let ts = take 4 $ iterate rotateArray array
  in ts <> map flipArray ts

tileOrientations :: Tile -> [Tile]
tileOrientations (Tile tileId tileArray) =
  map (Tile tileId) $ orientations tileArray

rotateArray :: A.Array (Int, Int) a -> A.Array (Int, Int) a
rotateArray tile =
  let (_, (yMax, xMax)) = A.bounds tile
  in A.array ((0, 0), (xMax, yMax)) $
    map (\((y, x), v) -> ((x, yMax-y), v)) (A.assocs tile)

flipArray :: A.Array (Int, Int) a -> A.Array (Int, Int) a
flipArray tile =
  let b@(_, (_, xMax)) = A.bounds tile
  in A.array b $ map (\((y, x), v) -> ((y, xMax-x), v)) (A.assocs tile)

getRange :: ((Int, Int), (Int, Int)) -> Tile -> [Char]
getRange r (Tile _ t) = map (t!) (A.range r)

readBinary :: [Char] -> Int
readBinary = foldl' (\x y -> x * 2 + y) 0 . map (fromEnum . (== '#'))

canonicalId :: [Char] -> Int
canonicalId ns = min (readBinary ns) (readBinary (reverse ns))

parseInput :: String -> IM.IntMap Tile
parseInput = IM.fromList . map parseTile . init . splitOn [""] . lines

parseTile :: [String] -> (Int, Tile)
parseTile (tileNumber:tileData) =
  let
    tileId = read $ take 4 $ drop 5 tileNumber
    size = length tileData
    tileArray = A.listArray ((0, 0), (size-1, size-1)) (concat tileData)
  in (tileId, Tile tileId tileArray)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn s t = splitOn' (t, [])
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t           = [reverse bs]
      | s `isPrefixOf` t = reverse bs : splitOn' (drop (length s) t, [])
      | otherwise        = splitOn' (xs, x:bs)
