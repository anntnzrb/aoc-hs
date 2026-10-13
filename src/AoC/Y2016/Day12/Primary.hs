{- |
approach: Interpret each instruction with four integer registers and a relative program counter.
cost: O(n + t log n) time, O(n) memory; one parse pass and two indexing passes for n instructions and t total executed steps across both runs.
trade: A direct interpreter preserves the instruction semantics without assuming a particular program; the two initial register states share the same parsed program.
-}
module AoC.Y2016.Day12.Primary (solver) where

import AoC.Assembunny (Instruction, Machine, Register (A, C), Value (RegisterValue), current, initialise, readValue, step)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day12.Input (Input (Input), parseInput)

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    pure (Answers (show (execute instructions [])) (Just (show (execute instructions [(C, 1)]))))

execute :: [Instruction] -> [(Register, Integer)] -> Integer
execute instructions initial =
    initialise instructions initial
        & iterate step
        & find halted
        & maybe 0 (readValue (RegisterValue A))
  where
    halted :: Machine -> Bool
    halted = current >>> isNothing
