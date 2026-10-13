-- | Nonempty instruction lines containing only the four keypad directions.
module AoC.Y2016.Day02.Input (Direction (Upward, Downward, Leftward, Rightward), Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char, eol)

data Direction = Upward | Downward | Leftward | Rightward

newtype Input = Input (NonEmpty (NonEmpty Direction))

parseInput :: Text -> Either Text Input
parseInput = parseText instructions

instructions :: Parser Input
instructions = Input <$> ((:|) <$> line <*> many (try (eol *> line))) <* optional eol

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
