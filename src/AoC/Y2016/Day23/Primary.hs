{- |
approach: Interpret jumps and toggles, collapsing verified terminating arithmetic loops in the live program.
cost: O(n + s log n) time and O(n) memory for n instructions and s interpreted or collapsed steps across both runs; one parse pass and two indexing passes, with arbitrary-precision arithmetic costs additional.
trade: Shared runtime pattern checks avoid repeated adjustments while preserving toggles, register aliasing and signed counters; unmatched loops retain direct execution.
-}
module AoC.Y2016.Day23.Primary (solver) where

import AoC.Assembunny (Instruction, Register (A), Value (RegisterValue), acceleratedStep, current, initialise, readValue)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day23.Input (Input (Input), parseInput)

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    answers (show (execute instructions 7)) (Just (show (execute instructions 12)))

execute :: [Instruction] -> Integer -> Integer
execute instructions initial =
    initialise instructions [(A, initial)]
        & iterate acceleratedStep
        & find (current >>> isNothing)
        & maybe 0 (readValue (RegisterValue A))
