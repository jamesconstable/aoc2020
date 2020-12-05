import Data.Char (isDigit)
import Data.Map (Map, (!), fromList, member)
import Text.Read (readMaybe)

import Runner (runner)

{-|
   Solver for Day 4 of the Advent of Code 2020
   Problem description: https://adventofcode.com/2020/day/4
-}

main :: IO ()
main = runner (solve False) (solve True)

solve :: Bool -> String -> Int
solve isPart2 =
  let
    requiredFields = [
      ("byr", validateInt 4 1920 2002),
      ("iyr", validateInt 4 2010 2020),
      ("eyr", validateInt 4 2020 2030),
      ("hgt", validateHeight),
      ("hcl", validateHair),
      ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
      ("pid", validateInt 9 minBound maxBound)]
    removeMissing = filter $ \p -> all ((`member` p) . fst) requiredFields
    removeInvalid = filter $ \p -> all (\(k, fn) -> fn (p!k)) requiredFields
  in length . (if isPart2 then removeInvalid else id) . removeMissing . parse

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
    _              -> False

validateHair :: String -> Bool
validateHair v =
  length v == 7 && head v == '#' && all (`elem` "0123456789abcdef") (tail v)

parse :: String -> [Map String String]
parse input = parse' [] (words <$> lines input)
  where
    toPairs = map ((\(k, ':':v) -> (k, v)) . break (== ':'))
    parse' acc ts = case ts of
      []            -> [fromList acc]
      ([]:rest)     -> fromList acc : parse' [] rest
      (fields:rest) -> parse' (toPairs fields ++ acc) rest
