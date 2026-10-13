-- | Basic Assembunny: register destinations and literal relative jump offsets.
module AoC.Y2016.Day12.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (Basic), Instruction, parseProgram)

-- | Basic Assembunny instructions in address order, starting at zero.
newtype Input
    = -- | Instructions in initial address order.
      Input [Instruction]

{- | Accept nonempty Basic Assembunny lines with register destinations and literal jump
offsets; reject unsupported opcodes or operands. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseProgram Basic >>> fmap Input
