-- | A nonempty row of safe tiles and traps, with an optional final newline.
module AoC.Y2016.Day18.Input (Input (Input), Tile (Safe, Trap), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse)
import Text.Megaparsec.Char (char, newline)

data Tile = Safe | Trap

newtype Input = Input (NonEmpty Tile)

parseInput :: Text -> Either Text Input
parseInput = parse row "input" >>> first (toText . errorBundlePretty)
  where
    row :: Parsec Void Text Input
    row = Input <$> ((:|) <$> tile <*> many tile) <* optional newline <* eof

    tile :: Parsec Void Text Tile
    tile = Safe <$ char '.' <|> Trap <$ char '^'
