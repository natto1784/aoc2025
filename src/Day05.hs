import qualified AoC as A (extract)
import Data.List (sort)
import Text.Parsec (char, digit, many1, newline, parse, sepEndBy, sepEndBy1)
import Text.Parsec.String (Parser)

type Id = Int

type Range = (Id, Id)

parseInput :: Parser ([Range], [Id])
parseInput = (,) <$> sepEndBy1 parseRange newline <* newline <*> sepEndBy parseId newline
  where
    parseRange :: Parser Range
    parseRange = (,) <$> parseId <* char '-' <*> parseId

    parseId :: Parser Id
    parseId = read <$> many1 digit

part1 :: [Range] -> [Id] -> Int
part1 ranges = sum . map (fromEnum . fresh)
  where
    fresh :: Id -> Bool
    fresh id = or [id >= a && id <= b | (a, b) <- ranges]

part2 :: [Range] -> Int
part2 =
  sum . map (\(l, r) -> r - l + 1) . foldr step [] . sort
  where
    step :: Range -> [Range] -> [Range]
    step lst [] = [lst]
    step (l1, r1) acc@((l2, r2) : xs)
      | r1 < l2 = (l1, r1) : acc
      | otherwise = (l1, max r1 r2) : xs

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day5.in"
    let (ranges, ids) = A.extract $ parse parseInput "" raw
    putStr "Part 1: " >> print (part1 ranges ids)
    putStr "Part 2: " >> print (part2 ranges)
