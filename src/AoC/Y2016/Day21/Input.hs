{- | Password operations retain written directions and non-negative positions.
Reversal spans are ordered; letters are lowercase ASCII.
-}
module AoC.Y2016.Day21.Input (Operation (SwapPosition, SwapLetter, Rotate, RotateLetter, Reverse, Move), Direction (Leftward, Rightward), Letter, letterValue, Span, bounds, parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (choice, satisfy, sepEndBy1, try)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | Which way a cyclic password rotation moves letters.
data Direction
    = -- | Move letters toward smaller indices.
      Leftward
    | -- | Move letters toward larger indices.
      Rightward

-- | A lowercase ASCII password character.
newtype Letter
    = Letter Char

-- | Recover the lowercase ASCII character represented by the letter.
letterValue :: Letter -> Char
letterValue (Letter value) = value

-- | An inclusive interval of zero-based positions with start no greater than end.
data Span
    = Span Natural Natural

-- | Return the inclusive start and end positions, in that order.
bounds :: Span -> (Natural, Natural)
bounds (Span start end) = (start, end)

spanBetween :: Natural -> Natural -> Either Text Span
spanBetween start end
    | start <= end = Right (Span start end)
    | otherwise = Left "reversed interval"

{- | One password edit; positions are zero-based and validated against the password
during execution.
-}
data Operation
    = -- | Exchange the letters at two positions.
      SwapPosition Natural Natural
    | -- | Exchange all occurrences of two letters.
      SwapLetter Letter Letter
    | -- | Rotate cyclically in a direction by a step count.
      Rotate Direction Natural
    | -- | Rotate right by one plus the letter index, plus one when that index is at least four.
      RotateLetter Letter
    | -- | Reverse an inclusive span of positions.
      Reverse Span
    | -- | Remove the letter at the first position and insert it at the second.
      Move Natural Natural

{- | Accept the six password operation grammars in written order; reject non-ASCII
letters and reversed spans. Positions remain unchecked against a password. Errors are
returned as text.
-}
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
