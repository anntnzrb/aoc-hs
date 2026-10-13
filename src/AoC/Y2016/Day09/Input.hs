-- | Whitespace-free compressed chunks; marker payloads remain literal text.
module AoC.Y2016.Day09.Input (Input (Input), Chunk (Literal, Repeat), parseInput) where

import AoC.Parse (Parser, boundedDecimal, parseText)
import Data.Char (isSpace)
import Data.Text qualified as T
import Text.Megaparsec (takeP, takeWhile1P)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Compressed chunks in source order after whitespace removal.
newtype Input
    = -- | Compressed chunks in source order.
      Input [Chunk]

-- | Literal text or a repeated payload whose nested markers have not yet been parsed.
data Chunk
    = -- | Text copied without repetition.
      Literal Text
    | -- | Copy count followed by raw marker payload.
      Repeat Natural Text

{- | Discard whitespace, then accept literals and (width x copies) markers without
spaces; reject malformed markers or truncated payloads. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput text =
    text
        & T.filter (not . isSpace)
        & parseText (Input <$> many chunk)

chunk :: Parser Chunk
chunk = repeated <|> (Literal <$> takeWhile1P (Just "literal") (/= '('))

repeated :: Parser Chunk
repeated = do
    width <- char '(' *> boundedDecimal
    copies <- char 'x' *> Lexer.decimal <* char ')'
    Repeat copies <$> takeP (Just "marker payload") width
