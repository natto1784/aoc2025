import qualified AoC as A (extract)
import Data.List (nub)
import Text.Parsec (char, digit, many1, parse, sepEndBy1)
import Text.Parsec.String (Parser)

type Id = Int

type Range = (Id, Id)

parseRanges :: Parser [Range]
parseRanges = sepEndBy1 parseRange (char ',')
  where
    parseRange :: Parser Range
    parseRange = liftA2 (,) parseId (char '-' *> parseId)

    parseId :: Parser Id
    parseId = read <$> many1 digit

-- Imagine the number is of form xx where x is the repeated k digit number
-- Note that xx is of length 2 * k, since x is of length k, and k is log_10(x)
-- The number will always be of the form x + (10 ^ k) * x or x * (10 ^ k + 1)
-- Hence, in modular arithmetic, xx = x (mod (10 ^ k + 1))
part1 :: [Range] -> Int
part1 = sum . map invalids
  where
    invalids :: Range -> Int
    invalids (l, r) =
      let len = length . show $ r
          reps = 2
          k = len `div` reps
          m = 10 ^ k + 1
       in sum
            [ xx
            | x <-
                [ ceiling (fromIntegral l / fromIntegral m :: Float)
                  .. floor (fromIntegral r / fromIntegral m :: Float)
                ],
              let xx = x * m,
              length (show xx) `mod` reps == 0
            ]

-- Second part is similar to first part except that it is now a geometric
-- progression of form x + (10 ^ k) * x .. (10 ^ (n - 1) * k) * x, where n is the
-- number of repetitions (2 in part1). So using similar logic and sum for
-- geometric progression we get:
part2 :: [Range] -> Int
part2 = sum . map invalids
  where
    invalids :: Range -> Int
    invalids (l, r) =
      let len = length . show $ r
       in sum . nub $
            [ xx
            | reps <- [2 .. len],
              let k = len `div` reps
                  m = (10 ^ (reps * k) - 1) `div` (10 ^ k - 1),
              x <-
                [ ceiling (fromIntegral l / fromIntegral m :: Float)
                  .. floor (fromIntegral r / fromIntegral m :: Float)
                ],
              let xx = x * m,
              length (show xx) `mod` reps == 0
            ]

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day2.in"
    let ranges = A.extract $ parse parseRanges "" raw
    putStr "Part 1: " >> print (part1 ranges)
    putStr "Part 2: " >> print (part2 ranges)
