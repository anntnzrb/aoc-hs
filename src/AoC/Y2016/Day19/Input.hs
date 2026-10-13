-- | A positive population, parsed as a decimal with an optional final newline.
module AoC.Y2016.Day19.Input (Input (elfCount), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Input = Input {elfCount :: Integer}

parseInput :: Text -> Either Text Input
parseInput = parseText population

population :: Parser Input
population = do
    count <- Lexer.decimal
    when (count == 0) (fail "the circle must contain at least one elf")
    _ <- optional eol
    pure (Input count)
