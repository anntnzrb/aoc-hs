-- | Screen instructions with bounded dimensions and indices; rotation amounts retain their written value.
module AoC.Y2016.Day08.Input (
    Instruction (Rect, RotateRow, RotateColumn),
    Width (width),
    Height (height),
    Row (row),
    Column (column),
    parseInput,
) where

import AoC.Parse (Parser, boundedDecimal, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Width = Width {width :: Int}
newtype Height = Height {height :: Int}
newtype Row = Row {row :: Int}
newtype Column = Column {column :: Int}

data Instruction = Rect Width Height | RotateRow Row Natural | RotateColumn Column Natural

parseInput :: Text -> Either Text [Instruction]
parseInput = parseText (instruction `sepEndBy1` eol)

instruction :: Parser Instruction
instruction =
    (Rect <$> (string "rect " *> (Width <$> bounded 50)) <* char 'x' <*> (Height <$> bounded 6))
        <|> (RotateRow <$> (string "rotate row y=" *> (Row <$> bounded 5)) <* string " by " <*> Lexer.decimal)
        <|> (RotateColumn <$> (string "rotate column x=" *> (Column <$> bounded 49)) <* string " by " <*> Lexer.decimal)

bounded :: Int -> Parser Int
bounded limit = do
    value <- boundedDecimal
    withinBounds value
  where
    withinBounds :: Int -> Parser Int
    withinBounds value
        | value <= limit = pure value
        | otherwise = fail "Outside screen bounds"
