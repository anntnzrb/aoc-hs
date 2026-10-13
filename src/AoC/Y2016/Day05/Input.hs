-- | A non-empty door identifier on one line, with an optional line ending.
module AoC.Y2016.Day05.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

-- | The door identifier, hashed verbatim after UTF-8 encoding by the solver.
newtype Input
    = -- | Unmodified door identifier.
      Input Text

{- | Accept a nonempty single-line door identifier and optional final line ending;
reject extra lines. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText doorId
  where
    doorId :: Parser Input
    doorId = Input <$> takeWhile1P (Just "door identifier") (\c -> c /= '\n' && c /= '\r') <* optional eol
