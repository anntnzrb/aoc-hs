-- | Whitespace-free compressed chunks; marker payloads remain literal text.
module AoC.Y2016.Day09.Input (Input (Input), Chunk (Literal, Repeat), parseInput) where

import AoC.Parse (Parser, boundedDecimal, parseText)
import Data.Char (isSpace)
import Text.Megaparsec (takeP, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Input = Input [Chunk]

data Chunk = Literal Text | Repeat Natural Text

parseInput :: Text -> Either Text Input
parseInput text =
    text
        & toString
        & filter (not . isSpace)
        & toText
        & parseText (Input <$> many chunk)

chunk :: Parser Chunk
chunk = repeated <|> (Literal <$> takeWhile1P (Just "literal") (/= '('))

repeated :: Parser Chunk
repeated = do
    width <- char '(' *> boundedDecimal
    copies <- char 'x' *> Lexer.decimal <* char ')'
    Repeat copies <$> takeP (Just "marker payload") width
