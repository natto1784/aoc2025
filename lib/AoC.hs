module AoC where

import Text.Parsec (ParseError)

-- extract Right value after parsing
extract :: Either ParseError a -> a
extract (Left err) = error ("Parsing failed: " ++ show err)
extract (Right val) = val
