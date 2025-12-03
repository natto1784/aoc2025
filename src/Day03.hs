import qualified AoC as A (extract)
import Data.Char (digitToInt)
import Text.Parsec (digit, many1, newline, parse, sepEndBy1)
import Text.Parsec.String (Parser)

type Battery = Int

type Bank = [Battery]

parseBanks :: Parser [Bank]
parseBanks = sepEndBy1 parseBank newline
  where
    parseBank :: Parser Bank
    parseBank = many1 parseBattery

    parseBattery :: Parser Battery
    parseBattery = digitToInt <$> digit

joltages :: Int -> [Bank] -> Int
joltages n = sum . map (joltage n)
  where
    joltage :: Int -> Bank -> Int
    joltage 1 xs = maximum xs
    joltage k xs =
      let len = length xs
          window = take (len - k + 1) xs
          (mx, (* (-1)) -> i) = maximum $ zip window [-1, -2 ..]
       in mx * 10 ^ (k - 1) + joltage (k - 1) (drop i xs)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day3.in"
    let banks = A.extract $ parse parseBanks "" raw
    putStr "Part 1: " >> print (joltages 2 banks)
    putStr "Part 2: " >> print (joltages 12 banks)
