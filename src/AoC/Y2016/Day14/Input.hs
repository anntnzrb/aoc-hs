-- | A non-empty salt on one line, with an optional line ending.
module AoC.Y2016.Day14.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

newtype Input = Input Text

parseInput :: Text -> Either Text Input
parseInput = parseText salt
  where
    salt :: Parser Input
    salt = Input <$> takeWhile1P (Just "salt") (\c -> c /= '\n' && c /= '\r') <* optional eol
