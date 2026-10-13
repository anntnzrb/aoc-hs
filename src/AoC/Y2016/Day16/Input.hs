-- | A nonempty binary initial state; parsing accepts one optional final newline.
module AoC.Y2016.Day16.Input (Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, eol)

newtype Input = Input (NonEmpty Bool)

parseInput :: Text -> Either Text Input
parseInput = parseText initialState

initialState :: Parser Input
initialState = Input <$> ((:|) <$> bit <*> many bit) <* optional eol
  where
    bit :: Parser Bool
    bit = False <$ char '0' <|> True <$ char '1'
