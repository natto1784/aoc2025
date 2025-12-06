import Data.Bifunctor (Bifunctor (bimap))
import Data.List (transpose)

data Operation = Add | Multiply deriving (Show)

type ProblemRaw = ([String], Operation)

type Problem = ([Int], Operation)

-- We are not going to use Parsec this time, so let's just group up the columns manually based on the
-- operator's index then we can simply use transpose later for part2 as well

parse :: String -> [ProblemRaw]
parse raw =
  let ls = lines raw
      opLine = last ls
      numLines = init ls

      ops :: [(Operation, Int)]
      ops =
        [ (if c == '+' then Add else Multiply, i)
        | (c, i) <- zip opLine [0 ..],
          c /= ' '
        ]

      starts = map snd ops
      slices line =
        [ take (next - start - 1) (drop start line)
        | (start, next) <- zip starts (drop 1 starts ++ [length line + 1])
        ]
   in zip (transpose (map slices numLines)) (map fst ops)

solve :: [Problem] -> Int
solve = sum . map solveProblem
  where
    solveProblem :: Problem -> Int
    solveProblem (nums, Multiply) = product nums
    solveProblem (nums, Add) = sum nums

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day6.in"
    let rawProblems = parse raw
        problems1 = map (bimap (map read) id) rawProblems
        problems2 = map (bimap (map read . transpose) id) rawProblems
    putStr "Part 1: " >> print (solve $ problems1)
    putStr "Part 2: " >> print (solve $ problems2)
