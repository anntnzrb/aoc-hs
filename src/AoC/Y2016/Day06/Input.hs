-- | Nonempty, equal-width messages containing any characters except CR and LF.
module AoC.Y2016.Day06.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (satisfy, try)
import Text.Megaparsec.Char (eol)

-- | Equal-width received messages, preserving row and character order.
newtype Input
    = -- | Messages in received order.
      Input (NonEmpty (NonEmpty Char))

{- | Accept nonempty equal-width messages, with CR and LF reserved for line endings;
reject ragged rows and blank lines. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText messages

messages :: Parser Input
messages = do
    rows <- (:|) <$> message <*> many (try (eol *> message)) <* optional eol
    unless (all ((== length (head rows)) . length) rows) (fail "messages must have equal widths")
    pure (Input rows)

message :: Parser (NonEmpty Char)
message = (:|) <$> character <*> many character
  where
    character :: Parser Char
    character = satisfy (\c -> c /= '\r' && c /= '\n')
