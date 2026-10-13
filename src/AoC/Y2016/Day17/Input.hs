-- | A non-empty vault passcode on one line, with an optional line ending.
module AoC.Y2016.Day17.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (takeWhile1P)
import Text.Megaparsec.Char (eol)

-- | The passcode prefixed to each movement path before hashing.
newtype Input
    = -- | Unmodified vault passcode.
      Input Text

{- | Accept a nonempty single-line passcode and optional final line ending; reject empty
passcodes or extra lines. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText passcode
  where
    passcode :: Parser Input
    passcode = Input <$> takeWhile1P (Just "passcode") (\c -> c /= '\n' && c /= '\r') <* optional eol
