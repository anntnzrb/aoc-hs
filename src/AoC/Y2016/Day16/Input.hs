-- | A nonempty binary initial state; parsing accepts one optional final newline.
module AoC.Y2016.Day16.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, eol)

-- | Seed bits in written order; False represents 0 and True represents 1.
newtype Input
    = -- | Initial bits, from left to right.
      Input (NonEmpty Bool)

{- | Accept a nonempty binary seed and optional final line ending; reject any character
other than 0 or 1. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText initialState

initialState :: Parser Input
initialState = Input <$> ((:|) <$> bit <*> many bit) <* optional eol
  where
    bit :: Parser Bool
    bit = False <$ char '0' <|> True <$ char '1'
