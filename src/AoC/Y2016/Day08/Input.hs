-- | Screen instructions; dimensions and indices are bounded when parsed.
module AoC.Y2016.Day08.Input (Instruction (Rect, RotateRow, RotateColumn), parseInput) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Instruction = Rect Int Int | RotateRow Int Int | RotateColumn Int Int

type Parser = Parsec Void Text

parseInput :: Text -> Either Text [Instruction]
parseInput = parse (instruction `sepEndBy1` eol <* eof) "screen" >>> first (errorBundlePretty >>> toText)

instruction :: Parser Instruction
instruction =
    (Rect <$> (string "rect " *> bounded 50) <* char 'x' <*> bounded 6)
        <|> (RotateRow <$> (string "rotate row y=" *> bounded 5) <* string " by " <*> shift 50)
        <|> (RotateColumn <$> (string "rotate column x=" *> bounded 49) <* string " by " <*> shift 6)

bounded :: Int -> Parser Int
bounded limit = do
    value <- (Lexer.decimal :: Parser Integer)
    if value <= toInteger limit then pure (fromInteger value) else fail "Outside screen bounds"

shift :: Int -> Parser Int
shift modulus = (Lexer.decimal :: Parser Integer) <&> (fromInteger . (`mod` toInteger modulus))
