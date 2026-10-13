-- | A non-empty door identifier on one line, with an optional line ending.
module AoC.Y2016.Day05.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

newtype Input = Input Text

parseInput :: Text -> Either Text Input
parseInput = parseText doorId
  where
    doorId :: Parser Input
    doorId = Input <$> takeWhile1P (Just "door identifier") (\c -> c /= '\n' && c /= '\r') <* optional eol
