-- | A non-negative designer number, parsed with at most one final newline.
module AoC.Y2016.Day13.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Input = Input Natural

parseInput :: Text -> Either Text Input
parseInput = parseText input
  where
    input :: Parser Input
    input = Input <$> Lexer.decimal <* optional eol
