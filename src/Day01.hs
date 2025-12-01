import qualified AoC as A (extract)
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy1, (<|>))
import Text.Parsec.String (Parser)

data Rotation = L Int | R Int

parseRotations :: Parser [Rotation]
parseRotations =
  sepEndBy1
    ( (char 'L' >> (L . read <$> many1 digit))
        <|> (char 'R' >> (R . read <$> many1 digit))
    )
    newline

part1 :: [Rotation] -> Int
part1 = length . filter (== 0) . scanl applyRotation 50
  where
    applyRotation :: Int -> Rotation -> Int
    applyRotation d (L x) = (d - x) `mod` 100
    applyRotation d (R x) = (d + x) `mod` 100

part2 :: [Rotation] -> Int
part2 = snd . foldl applyRotation (50, 0)
  where
    applyRotation :: (Int, Int) -> Rotation -> (Int, Int)
    applyRotation (d, z) (L x) = ((d - x) `mod` 100, z + ((100 - d) `mod` 100 + x) `div` 100)
    applyRotation (d, z) (R x) = ((d + x) `mod` 100, z + (d + x) `div` 100)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day1.in"
    let rotations = A.extract $ parse parseRotations "" raw
    putStr "Part 1: " >> print (part1 rotations)
    putStr "Part 2: " >> print (part2 rotations)
