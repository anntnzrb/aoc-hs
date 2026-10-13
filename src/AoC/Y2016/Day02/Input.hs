-- | Nonempty instruction lines containing only the four keypad directions.
module AoC.Y2016.Day02.Input (Direction (Upward, Downward, Leftward, Rightward), Input (Input), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, try)
import Text.Megaparsec.Char (char, eol)

data Direction = Upward | Downward | Leftward | Rightward

newtype Input = Input (NonEmpty (NonEmpty Direction))

type Parser = Parsec Void Text

parseInput :: Text -> Either Text Input
parseInput = parse instructions "instructions" >>> first (toText . errorBundlePretty)

instructions :: Parser Input
instructions = Input <$> ((:|) <$> line <*> many (try (eol *> line))) <* optional eol <* eof

line :: Parser (NonEmpty Direction)
line = (:|) <$> direction <*> many direction

direction :: Parser Direction
direction =
    Upward
        <$ char 'U'
            <|> Downward
        <$ char 'D'
            <|> Leftward
        <$ char 'L'
            <|> Rightward
        <$ char 'R'
