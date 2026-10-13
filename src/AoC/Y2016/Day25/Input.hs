-- | Clock Assembunny, including toggles and integer output operands.
module AoC.Y2016.Day25.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (Clock), Instruction, parseProgram)

newtype Input = Input [Instruction]

parseInput :: Text -> Either Text Input
parseInput = parseProgram Clock >>> fmap Input
