-- | Basic Assembunny: register destinations and literal relative jump offsets.
module AoC.Y2016.Day12.Input (Input (Input), parseInput) where

import AoC.Assembunny (Dialect (Basic), Instruction, parseProgram)

newtype Input = Input [Instruction]

parseInput :: Text -> Either Text Input
parseInput = parseProgram Basic >>> fmap Input
