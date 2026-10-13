-- | A nonempty row of safe tiles and traps, with an optional final newline.
module AoC.Y2016.Day18.Input (Input (Input), Tile (Safe, Trap), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, newline)

-- | Whether a tile may be stepped on or is a trap.
data Tile
    = -- | A dot tile, safe to step on.
      Safe
    | -- | A caret tile, unsafe to step on.
      Trap

-- | The initial tile row from left to right.
newtype Input
    = -- | Initial row in left-to-right order.
      Input (NonEmpty Tile)

{- | Accept one nonempty row of dots and carets with optional final LF; reject other
tiles or extra rows. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText row
  where
    row :: Parser Input
    row = Input <$> ((:|) <$> tile <*> many tile) <* optional newline

    tile :: Parser Tile
    tile = Safe <$ char '.' <|> Trap <$ char '^'
