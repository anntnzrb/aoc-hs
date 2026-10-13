{- |
approach: Interpret each instruction, collapsing verified terminating arithmetic loops in the live program.
cost: O(n + s log n) time, O(n) memory; one parse pass and two indexing passes for n instructions and s interpreted or collapsed steps across both runs, with integer arithmetic costs additional.
trade: Shared loop checks replace repeated adjustments with exact arithmetic; unmatched instructions retain direct semantics and both initial register states share the parsed program.
-}
module AoC.Y2016.Day12.Primary (solver) where

import AoC.Assembunny (Instruction, Machine, Register (A, C), Value (RegisterValue), acceleratedStep, current, initialise, readValue)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day12.Input (Input (Input), parseInput)

{- | Return register a after execution with c initially 0 and 1; other registers start
at zero. Both initial states require a halting program; execution such as @jnz 1 0@
never returns. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input instructions <- parseInput text
    answers (show (execute instructions [])) (Just (show (execute instructions [(C, 1)])))

-- | Read register a after halting; requires the program to halt from this state.
execute :: [Instruction] -> [(Register, Integer)] -> Integer
execute instructions initial =
    initialise instructions initial
        & iterate acceleratedStep
        & find halted
        & maybe 0 (readValue (RegisterValue A))
  where
    halted :: Machine -> Bool
    halted = current >>> isNothing
