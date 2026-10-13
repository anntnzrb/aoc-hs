-- | A nonempty binary initial state; parsing accepts one optional final newline.
module AoC.Y2016.Day16.Input (Input (Input), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse)
import Text.Megaparsec.Char (char, eol)

newtype Input = Input (NonEmpty Bool)

parseInput :: Text -> Either Text Input
parseInput = parse initialState "initial state" >>> first (toText . errorBundlePretty)

initialState :: Parsec Void Text Input
initialState = Input <$> ((:|) <$> bit <*> many bit) <* optional eol <* eof
  where
    bit :: Parsec Void Text Bool
    bit = False <$ char '0' <|> True <$ char '1'
