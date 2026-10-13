-- | Inclusive, ordered ranges of unsigned 32-bit addresses.
module AoC.Y2016.Day20.Input (Range, bounds, addressCount, parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Range = Range Integer Integer

bounds :: Range -> (Integer, Integer)
bounds (Range lower upper) = (lower, upper)

addressCount :: Integer
addressCount = 4294967296

parseInput :: Text -> Either Text [Range]
parseInput = parse (range `sepEndBy1` eol <* eof) "blacklist" >>> first (toText . errorBundlePretty)

range :: Parsec Void Text Range
range = do
    lower <- decimal
    upper <- char '-' *> decimal
    if lower <= upper && upper < addressCount
        then pure (Range lower upper)
        else fail "expected an ordered range of unsigned 32-bit addresses"
