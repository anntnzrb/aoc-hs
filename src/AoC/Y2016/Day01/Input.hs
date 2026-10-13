{- | Nonempty, comma-space-separated turns with nonnegative block counts.
Parsing consumes the whole input, permitting one trailing line ending.
-}
module AoC.Y2016.Day01.Input (Input (Input), Instruction (Instruction), Turn (TurnLeft, TurnRight), Steps (Steps), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Turn = TurnLeft | TurnRight

newtype Steps = Steps Natural

data Instruction = Instruction Turn Steps

newtype Input = Input (NonEmpty Instruction)

parseInput :: Text -> Either Text Input
parseInput = parseText instructions

instructions :: Parser Input
instructions = Input <$> ((:|) <$> instruction <*> many (string ", " *> instruction)) <* optional eol

instruction :: Parser Instruction
instruction = Instruction <$> turn <*> (Steps <$> Lexer.decimal)
  where
    turn :: Parser Turn
    turn = TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'
