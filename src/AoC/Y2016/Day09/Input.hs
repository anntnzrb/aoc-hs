-- | Whitespace-free compressed chunks; marker payloads remain literal text.
module AoC.Y2016.Day09.Input (Input (Input), Chunk (Literal, Repeat), parseInput) where

import Data.Char (isSpace)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, takeP, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Input = Input [Chunk]

data Chunk = Literal Text | Repeat Natural Text

type Parser = Parsec Void Text

parseInput :: Text -> Either Text Input
parseInput text =
    text
        & toString
        & filter (not . isSpace)
        & toText
        & parse (Input <$> many chunk <* eof) "compressed input"
        & first (toText . errorBundlePretty)

chunk :: Parser Chunk
chunk = repeated <|> (Literal <$> takeWhile1P (Just "literal") (/= '('))

repeated :: Parser Chunk
repeated = do
    width <- char '(' *> (Lexer.decimal :: Parser Integer)
    copies <- char 'x' *> Lexer.decimal <* char ')'
    when (width > toInteger (maxBound @Int)) (fail "marker width exceeds input capacity")
    Repeat copies <$> takeP (Just "marker payload") (fromInteger width)
