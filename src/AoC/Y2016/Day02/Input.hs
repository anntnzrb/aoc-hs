-- | Nonempty instruction lines containing only the four keypad directions.
module AoC.Y2016.Day02.Input (Direction (Upward, Downward, Leftward, Rightward), Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (char, eol)

-- | A unit keypad movement; the keypad decides whether its destination exists.
data Direction
    = -- | Move one row up.
      Upward
    | -- | Move one row down.
      Downward
    | -- | Move one column left.
      Leftward
    | -- | Move one column right.
      Rightward

-- | One nonempty movement sequence per requested keypad digit, in input order.
newtype Input
    = -- | Movement lines in code-digit order.
      Input (NonEmpty (NonEmpty Direction))

{- | Accept nonempty U/D/L/R lines, preserving line boundaries; reject blank lines and
other characters. Errors are returned as text.
-}
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
