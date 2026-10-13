{- | Screen instructions with bounded dimensions and indices; rotation amounts retain
their written value.
-}
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

-- | Rectangle width in pixels, from 0 through 50.
newtype Width = Width
    { width :: Int
    -- ^ Pixels across, from 0 through 50.
    }

-- | Rectangle height in pixels, from 0 through 6.
newtype Height = Height
    { height :: Int
    -- ^ Pixels down, from 0 through 6.
    }

-- | Zero-based row index, from 0 through 5.
newtype Row = Row
    { row :: Int
    -- ^ Row index, from 0 through 5.
    }

-- | Zero-based column index, from 0 through 49.
newtype Column = Column
    { column :: Int
    -- ^ Column index, from 0 through 49.
    }

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
