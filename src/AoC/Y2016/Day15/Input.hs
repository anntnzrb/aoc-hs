-- | Discs numbered by arrival second, with positive sizes and valid positions.
module AoC.Y2016.Day15.Input (Disc (number, positions, initial), disc, parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Disc = Disc
    { number :: Integer
    , positions :: Integer
    , initial :: Integer
    }

disc :: Integer -> Integer -> Integer -> Either Text Disc
disc number positions initial
    | number <= 0 = Left "Disc numbers must be positive"
    | positions <= 0 = Left "Disc sizes must be positive"
    | initial < 0 || initial >= positions = Left "Position outside disc"
    | otherwise = Right (Disc number positions initial)

parseInput :: Text -> Either Text [Disc]
parseInput = parse (discParser `sepEndBy1` eol <* eof) "discs" >>> first (toText . errorBundlePretty)

discParser :: Parsec Void Text Disc
discParser = do
    number <- string "Disc #" *> decimal
    positions <- string " has " *> decimal
    initial <- string " positions; at time=0, it is at position " *> decimal <* string "."
    disc number positions initial & either (fail . toString) pure
