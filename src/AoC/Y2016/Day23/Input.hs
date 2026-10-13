-- | Self-modifying Assembunny, retaining literal destinations after toggles.
module AoC.Y2016.Day23.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (SelfModifying), Instruction, parseProgram)

-- | Self-modifying instructions in initial address order.
newtype Input
    = -- | Instructions in initial address order.
      Input [Instruction]

{- | Accept nonempty SelfModifying Assembunny lines, including toggle and literal
destinations; reject unsupported instructions or malformed operands. Errors are
returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput = parseProgram SelfModifying >>> fmap Input
