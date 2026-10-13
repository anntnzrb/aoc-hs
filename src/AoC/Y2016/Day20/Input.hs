-- | Inclusive, ordered ranges of unsigned 32-bit addresses.
module AoC.Y2016.Day20.Input (Range, bounds, addressCount, parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Range = Range Integer Integer

bounds :: Range -> (Integer, Integer)
bounds (Range lower upper) = (lower, upper)

addressCount :: Integer
addressCount = 4294967296

parseInput :: Text -> Either Text [Range]
parseInput = parseText (range `sepEndBy1` eol)

range :: Parser Range
range = do
    lower <- decimal
    upper <- char '-' *> decimal
    when (lower > upper || upper >= addressCount) (fail "expected an ordered range of unsigned 32-bit addresses")
    pure (Range lower upper)
