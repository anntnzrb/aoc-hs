-- | A non-empty vault passcode on one line, with an optional line ending.
module AoC.Y2016.Day17.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

newtype Input = Input Text

parseInput :: Text -> Either Text Input
parseInput = parseText passcode
  where
    passcode :: Parser Input
    passcode = Input <$> takeWhile1P (Just "passcode") (\c -> c /= '\n' && c /= '\r') <* optional eol
