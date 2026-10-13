-- | A positive population, parsed as a decimal with an optional final newline.
module AoC.Y2016.Day19.Input (Input (elfCount), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | A strictly positive population of elves numbered from 1.
newtype Input = Input
    { elfCount :: Integer
    -- ^ Positive count of elves, whose labels start at 1.
    }

{- | Accept one positive decimal population and optional final line ending; reject zero,
signs or trailing text. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText population

population :: Parser Input
population = do
    count <- Lexer.decimal
    when (count == 0) (fail "the circle must contain at least one elf")
    _ <- optional eol
    pure (Input count)
