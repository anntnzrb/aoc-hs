{- | Password operations retain written directions and non-negative positions.
Reversal spans are ordered; letters are lowercase ASCII.
-}
module AoC.Y2016.Day21.Input (Operation (SwapPosition, SwapLetter, Rotate, RotateLetter, Reverse, Move), Direction (Leftward, Rightward), Letter, letterValue, Span, bounds, parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (choice, satisfy, sepEndBy1, try)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Direction = Leftward | Rightward

newtype Letter = Letter Char

letterValue :: Letter -> Char
letterValue (Letter value) = value

data Span = Span Natural Natural

bounds :: Span -> (Natural, Natural)
bounds (Span start end) = (start, end)

spanBetween :: Natural -> Natural -> Either Text Span
spanBetween start end
    | start <= end = Right (Span start end)
    | otherwise = Left "reversed interval"

data Operation
    = SwapPosition Natural Natural
    | SwapLetter Letter Letter
    | Rotate Direction Natural
    | RotateLetter Letter
    | Reverse Span
    | Move Natural Natural

parseInput :: Text -> Either Text [Operation]
parseInput = parseText (operation `sepEndBy1` eol)

operation :: Parser Operation
operation = choice (map try [swapPosition, swapLetter, rotation, rotateLetter, reversal, movement])

letter :: Parser Letter
letter = Letter <$> satisfy isAsciiLower

swapPosition :: Parser Operation
swapPosition = SwapPosition <$> (string "swap position " *> Lexer.decimal) <*> (string " with position " *> Lexer.decimal)

swapLetter :: Parser Operation
swapLetter = SwapLetter <$> (string "swap letter " *> letter) <*> (string " with letter " *> letter)

rotation :: Parser Operation
rotation = Rotate <$> direction <*> Lexer.decimal <* (string " step" *> optional (char 's'))
  where
    direction :: Parser Direction
    direction = string "rotate " *> (Leftward <$ string "left " <|> Rightward <$ string "right ")

rotateLetter :: Parser Operation
rotateLetter = RotateLetter <$> (string "rotate based on position of letter " *> letter)

reversal :: Parser Operation
reversal = do
    start <- string "reverse positions " *> Lexer.decimal
    end <- string " through " *> Lexer.decimal
    spanBetween start end & either (fail . toString) (pure . Reverse)

movement :: Parser Operation
movement = Move <$> (string "move position " *> Lexer.decimal) <*> (string " to position " *> Lexer.decimal)
