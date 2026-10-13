-- | A non-empty salt on one line, with an optional line ending.
module AoC.Y2016.Day14.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

-- | The salt prefixed to decimal indices before hashing.
newtype Input
    = -- | Unmodified hashing salt.
      Input Text

{- | Accept a nonempty single-line salt and optional final line ending; reject empty
salts or extra lines. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText salt
  where
    salt :: Parser Input
    salt = Input <$> takeWhile1P (Just "salt") (\c -> c /= '\n' && c /= '\r') <* optional eol
