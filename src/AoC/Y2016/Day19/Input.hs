-- | A positive population, parsed as a decimal with an optional final newline.
module AoC.Y2016.Day19.Input (Input (elfCount), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Input = Input {elfCount :: Integer}

parseInput :: Text -> Either Text Input
parseInput = runParser population "elf population" >>> first (toText . errorBundlePretty)

population :: Parsec Void Text Input
population = do
    count <- Lexer.decimal
    when (count == 0) (fail "the circle must contain at least one elf")
    _ <- optional eol
    eof
    pure (Input count)
