import Data.Char (isDigit)
import Data.Map (findWithDefault, fromList, member, size)
import System.Environment (getArgs)
import Text.Read (readMaybe)

{-
   Solver for Day 4 of the Advent of Code 2020
   Problem description here: https://adventofcode.com/2020/day/4

   Takes a command line argument indicating which part to solve (1 or 2), reads
   input from stdin, and prints solution to stdout.
-}

data Field = Field { key :: String, value :: String }

main :: IO ()
main = do
  let run = (>>= putStrLn) . (<$> getContents) . (show .)
  part <- getArgs
  case part of
    ["1"]     -> run $ solve False
    ["2"]     -> run $ solve True
    otherwise -> putStrLn "Valid options are 1 or 2"

solve :: Bool -> String -> Int
solve validateFields =
  let
    requiredFields = fromList [
      ("byr", validateInt 4 1920 2002),
      ("iyr", validateInt 4 2010 2020),
      ("eyr", validateInt 4 2020 2030),
      ("hgt", validateHeight),
      ("hcl", validateHair),
      ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
      ("pid", validateInt 9 minBound maxBound)]
    isValid (Field k v) = findWithDefault (const False) k requiredFields v
    filterByFields fn = filter ((== size requiredFields) . length . filter fn)
  in length
    . (if validateFields then filterByFields isValid else id)
    . filterByFields ((`member` requiredFields) . key)
    . parseInput

inRange :: Ord a => a -> a -> a -> Bool
inRange min max v = min <= v && v <= max

validateInt :: Int -> Int -> Int -> String -> Bool
validateInt size min max v =
  length v == size && maybe False (inRange min max) (readMaybe v)

validateHeight :: String -> Bool
validateHeight v =
  let (digits, unit) = span isDigit v
  in case (readMaybe digits, unit) of
    (Just n, "cm") -> inRange 150 193 n
    (Just n, "in") -> inRange 59 76 n
    otherwise      -> False

validateHair :: String -> Bool
validateHair v =
  if length v == 7 && head v == '#'
  then all (`elem` "0123456789abcdef") (tail v)
  else False

parseInput :: String -> [[Field]]
parseInput input = parse [] (words <$> lines input)
  where
    toFields = map ((\(k, ':':v) -> Field k v) . break (== ':'))
    parse acc ts = case ts of
      []            -> [acc]
      ([]:rest)     -> acc : parse [] rest
      (fields:rest) -> parse (toFields fields ++ acc) rest
