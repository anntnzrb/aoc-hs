-- | Self-modifying Assembunny, retaining literal destinations after toggles.
module AoC.Y2016.Day23.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (SelfModifying), Instruction, parseProgram)

newtype Input = Input [Instruction]

parseInput :: Text -> Either Text Input
parseInput = parseProgram SelfModifying >>> fmap Input
