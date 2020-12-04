import Data.Array ((!), Array, array, bounds)
import Data.Char (isDigit)
import Data.Map (findWithDefault, fromList, member, size)
import System.Environment (getArgs)
import Text.Read (readMaybe)

{-
   Solver for Day 4 of the Advent of Code 2020
   Problem description here: https://adventofcode.com/2020/day/3

   Takes a command line argument indicating which part to solve (1 or 2), reads
   input from stdin, and prints solution to stdout.
-}

main :: IO ()
main = do
  let run = (>>= putStrLn) . (<$> getContents) . (show .)
  part <- getArgs
  case part of
    ["1"]     -> run $ solve False
    ["2"]     -> run $ solve True
    otherwise -> putStrLn "Valid options are 1 or 2"

solve :: Bool -> String -> Int
solve validateFields input =
  let
    checkRangedInt min max v = length v == 4 && read v >= min && read v <= max
    checkHeight v =
      let (digits, unit) = span isDigit v
      in case (readMaybe digits, unit) of
          (Just n, "cm") -> 150 <= n && n <= 193
          (Just n, "in") -> 59 <= n && n <= 76
          otherwise      -> False
    checkHair v =
      if length v == 7 && head v == '#'
      then all (`elem` "0123456789abcdef") (tail v)
      else False
    checkPID v = length v == 9 && all isDigit v
    requiredFields = fromList [
      ("byr", checkRangedInt 1920 2002),
      ("iyr", checkRangedInt 2010 2020),
      ("eyr", checkRangedInt 2020 2030),
      ("hgt", checkHeight),
      ("hcl", checkHair),
      ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
      ("pid", checkPID)]
    passports = parseInput input
    isRequired = (`member` requiredFields) . fst
    isValid (k, v) = findWithDefault (const False) k requiredFields v
    hasRequiredFields = (== size requiredFields) . length . filter isRequired
    hasValidFields = (== size requiredFields) . length . filter isValid
  in length
    $ (if validateFields then filter hasValidFields else id)
    $ filter hasRequiredFields passports

parseInput :: String -> [[(String, String)]]
parseInput input = parse [] (words <$> lines input)
  where
    toPairs = map ((\(k, ':':v) -> (k, v)) . break (== ':'))
    parse acc ts = case ts of
      []            -> [acc]
      ([]:rest)     -> acc : parse [] rest
      (fields:rest) -> parse (toPairs fields ++ acc) rest
