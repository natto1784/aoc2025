import qualified Data.Set as S (Set, difference, empty, foldr', fromList, insert, member, null, size)

type Roll = (Int, Int)

type Rolls = S.Set Roll

accessible :: Rolls -> Rolls
accessible rolls =
  S.foldr'
    (\p acc -> if reachable p <= 4 then S.insert p acc else acc)
    S.empty
    rolls
  where
    reachable (x, y) =
      length
        [ ()
        | dx <- [-1 .. 1],
          dy <- [-1 .. 1],
          let p' = (x + dx, y + dy),
          S.member p' rolls
        ]

part1 :: Rolls -> Int
part1 = S.size . accessible

part2 :: Rolls -> Int
part2 = go 0
  where
    go :: Int -> Rolls -> Int
    go n rolls =
      let accRolls = accessible rolls
       in if S.null accRolls
            then n
            else
              let remaining = rolls `S.difference` accRolls
               in go (n + S.size accRolls) remaining

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day4.in"
    let rolls =
          S.fromList
            [ (x, y)
            | (y, line) <- zip [0 ..] (lines raw),
              (x, ch) <- zip [0 ..] line,
              ch == '@'
            ]

    putStr "Part 1: " >> print (part1 rolls)
    putStr "Part 2: " >> print (part2 rolls)
