-- | A non-negative designer number, parsed with at most one final newline.
module AoC.Y2016.Day13.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | The nonnegative designer number used to classify maze coordinates.
newtype Input
    = -- | Designer number for maze classification.
      Input Natural

{- | Accept one unsigned decimal designer number and optional final line ending; reject
signs or extra text. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText input
  where
    input :: Parser Input
    input = Input <$> Lexer.decimal <* optional eol
