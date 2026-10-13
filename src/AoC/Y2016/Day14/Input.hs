-- | A non-empty salt on one line, with an optional line ending.
module AoC.Y2016.Day14.Input (Input (Input), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, takeWhile1P)
import Text.Megaparsec.Char (eol)

newtype Input = Input Text

parseInput :: Text -> Either Text Input
parseInput = parse salt "salt" >>> first (toText . errorBundlePretty)
  where
    salt :: Parsec Void Text Input
    salt = Input <$> takeWhile1P (Just "salt") (\c -> c /= '\n' && c /= '\r') <* optional eol <* eof
