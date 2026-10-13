{- | Nonempty, comma-space-separated turns with nonnegative block counts.
Parsing consumes the whole input, permitting one trailing line ending.
-}
module AoC.Y2016.Day01.Input (Input (Input), Instruction (Instruction), Turn (TurnLeft, TurnRight), Steps (Steps), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | A quarter turn relative to the current heading.
data Turn
    = -- | Rotate 90 degrees anticlockwise.
      TurnLeft
    | -- | Rotate 90 degrees clockwise.
      TurnRight

-- | A nonnegative distance in city blocks.
newtype Steps
    = -- | Distance in blocks.
      Steps Natural

-- | Turn first, then travel the given number of blocks.
data Instruction
    = -- | A turn followed by a distance.
      Instruction Turn Steps

-- | A nonempty route in written order, starting from the caller's chosen position.
newtype Input
    = -- | Instructions in travel order.
      Input (NonEmpty Instruction)

{- | Accept comma-space-separated L/R turns and nonnegative distances; reject empty
routes and trailing junk. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseText instructions

instructions :: Parser Input
instructions = Input <$> ((:|) <$> instruction <*> many (string ", " *> instruction)) <* optional eol

instruction :: Parser Instruction
instruction = Instruction <$> turn <*> (Steps <$> Lexer.decimal)
  where
    turn :: Parser Turn
    turn = TurnLeft <$ char 'L' <|> TurnRight <$ char 'R'
