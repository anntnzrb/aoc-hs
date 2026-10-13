-- | A nonempty row of safe tiles and traps, with an optional final newline.
module AoC.Y2016.Day18.Input (Input (Input), Tile (Safe, Trap), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, newline)

data Tile = Safe | Trap

newtype Input = Input (NonEmpty Tile)

parseInput :: Text -> Either Text Input
parseInput = parseText row
  where
    row :: Parser Input
    row = Input <$> ((:|) <$> tile <*> many tile) <* optional newline

    tile :: Parser Tile
    tile = Safe <$ char '.' <|> Trap <$ char '^'
