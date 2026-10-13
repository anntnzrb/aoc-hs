-- | Operations on eight distinct letters a through h; positions are in 0..7.
module AoC.Y2016.Day21.Input (Operation (SwapPosition, SwapLetter, Rotate, RotateLetter, Reverse, Move), parseInput) where

import Text.Megaparsec (Parsec, choice, eof, errorBundlePretty, runParser, satisfy, sepEndBy1, try)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Operation
    = SwapPosition Int Int
    | SwapLetter Char Char
    | Rotate Int
    | RotateLetter Char
    | Reverse Int Int
    | Move Int Int

type Parser = Parsec Void Text

parseInput :: Text -> Either Text [Operation]
parseInput = runParser (operation `sepEndBy1` eol <* eof) "operations" >>> first (errorBundlePretty >>> toText)

operation :: Parser Operation
operation = choice (map try [swapPosition, swapLetter, rotation, rotateLetter, reversal, movement])

position :: Parser Int
position = do
    value <- (Lexer.decimal :: Parser Integer)
    if value < 8 then pure (fromInteger value) else fail "position must be in 0..7"

letter :: Parser Char
letter = satisfy (\value -> value >= 'a' && value <= 'h')

swapPosition :: Parser Operation
swapPosition = SwapPosition <$> (string "swap position " *> position) <*> (string " with position " *> position)

swapLetter :: Parser Operation
swapLetter = SwapLetter <$> (string "swap letter " *> letter) <*> (string " with letter " *> letter)

rotation :: Parser Operation
rotation = do
    direction <- string "rotate " *> ((-1) <$ string "left " <|> 1 <$ string "right ")
    steps <- (Lexer.decimal :: Parser Integer)
    _ <- string " step" *> optional (char 's')
    pure (Rotate (direction * fromInteger (steps `mod` 8)))

rotateLetter :: Parser Operation
rotateLetter = RotateLetter <$> (string "rotate based on position of letter " *> letter)

reversal :: Parser Operation
reversal = do
    start <- string "reverse positions " *> position
    end <- string " through " *> position
    if start <= end then pure (Reverse start end) else fail "reversed interval"

movement :: Parser Operation
movement = Move <$> (string "move position " *> position) <*> (string " to position " *> position)
