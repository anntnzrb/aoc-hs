{- | Screen instructions with bounded dimensions and indices; rotation amounts retain
their written value.
-}
module AoC.Y2016.Day08.Input (
    Instruction (Rect, RotateRow, RotateColumn),
    Width,
    Height,
    Row,
    Column,
    width,
    height,
    row,
    column,
    parseInput,
) where

import AoC.Parse (Parser, boundedDecimal, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Rectangle width in pixels, from 0 through 50.
newtype Width = Width Int

-- | Read pixels across, from 0 through 50; cannot change the bounded value.
width :: Width -> Int
width (Width value) = value

-- | Rectangle height in pixels, from 0 through 6.
newtype Height = Height Int

-- | Read pixels down, from 0 through 6; cannot change the bounded value.
height :: Height -> Int
height (Height value) = value

-- | Zero-based row index, from 0 through 5.
newtype Row = Row Int

-- | Read the row index, from 0 through 5; cannot change the bounded value.
row :: Row -> Int
row (Row value) = value

-- | Zero-based column index, from 0 through 49.
newtype Column = Column Int

-- | Read the column index, from 0 through 49; cannot change the bounded value.
column :: Column -> Int
column (Column value) = value

-- | A screen edit; rotation counts wrap around the selected axis.
data Instruction
    = -- | Light the top-left rectangle with width then height.
      Rect Width Height
    | -- | Move row pixels right by the given count.
      RotateRow Row Natural
    | -- | Move column pixels down by the given count.
      RotateColumn Column Natural

{- | Accept rect and row/column rotation lines for a 50-by-6 screen; reject dimensions
or indices outside its bounds. Errors are returned as text.
-}
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
