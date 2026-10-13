-- | Clock Assembunny, including toggles and integer output operands.
module AoC.Y2016.Day25.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (Clock), Instruction, parseProgram)

-- | Clock instructions in initial address order; output operands are integers.
newtype Input
    = -- | Instructions in initial address order.
      Input [Instruction]

{- | Accept nonempty Clock Assembunny lines, including toggles and output; reject
unsupported instructions or malformed operands. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseProgram Clock >>> fmap Input
