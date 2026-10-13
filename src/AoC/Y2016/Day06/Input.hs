-- | Nonempty, equal-width messages containing lowercase ASCII letters.
module AoC.Y2016.Day06.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (satisfy, try)
import Text.Megaparsec.Char (eol)

newtype Input = Input (NonEmpty (NonEmpty Char))

parseInput :: Text -> Either Text Input
parseInput = parseText messages

messages :: Parser Input
messages = do
    rows <- (:|) <$> message <*> many (try (eol *> message)) <* optional eol
    unless (all ((== length (head rows)) . length) rows) (fail "messages must have equal widths")
    pure (Input rows)

message :: Parser (NonEmpty Char)
message = (:|) <$> letter <*> many letter
  where
    letter :: Parser Char
    letter = satisfy isAsciiLower
