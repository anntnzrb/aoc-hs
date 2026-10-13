-- | Inclusive, ordered ranges of unsigned 32-bit addresses.
module AoC.Y2016.Day20.Input (Range, bounds, addressCount, parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

-- | An inclusive blocked interval with ordered unsigned 32-bit endpoints.
data Range = Range Integer Integer

-- | Return inclusive lower and upper endpoints, in that order.
bounds :: Range -> (Integer, Integer)
bounds (Range lower upper) = (lower, upper)

-- | Number of addresses in the unsigned 32-bit space; the exclusive upper bound.
addressCount :: Integer
addressCount = 4294967296

{- | Accept nonempty lines of inclusive lower-upper decimal ranges; reject reversed
ranges or addresses outside unsigned 32-bit bounds. Errors are returned as text.
-}
parseInput :: Text -> Either Text [Range]
parseInput = parseText (range `sepEndBy1` eol)

range :: Parser Range
range = do
    lower <- decimal
    upper <- char '-' *> decimal
    when (lower > upper || upper >= addressCount) (fail "expected an ordered range of unsigned 32-bit addresses")
    pure (Range lower upper)
