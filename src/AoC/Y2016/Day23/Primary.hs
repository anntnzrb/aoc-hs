{- |
approach: Interpret relative jumps and toggles against the current program, preserving operands and skipping invalid writes.
cost: O(n + t log n) time and O(n) memory for n instructions and t executed steps; one parse pass and one indexing pass.
trade: Direct execution models self-modification without assumptions about the supplied program; part 2 is unfinished.
-}
module AoC.Y2016.Day23.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day23.Input (
    Input (Input),
    Instruction (Copy, Decrement, Increment, Jump, Toggle),
    Register (A),
    Value (Literal, RegisterValue),
    parseInput,
 )
import Data.Map.Strict qualified as Map

data Machine = Machine
    { counter :: Integer
    , registers :: Map Register Integer
    , program :: Map Integer Instruction
    }

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    pure (Answers (show (execute instructions 7)) (Just "0"))

execute :: [Instruction] -> Integer -> Integer
execute instructions initial =
    Machine 0 (one (A, initial)) (Map.fromDistinctAscList (zip [0 ..] instructions))
        & iterate step
        & find (\machine -> Map.notMember machine.counter machine.program)
        & maybe 0 (readValue (RegisterValue A))

step :: Machine -> Machine
step machine = maybe machine (apply machine) (Map.lookup machine.counter machine.program)

apply :: Machine -> Instruction -> Machine
apply machine = \case
    Copy source destination -> advance (writeValue destination (readValue source machine) machine)
    Increment destination -> advance (writeValue destination (readValue destination machine + 1) machine)
    Decrement destination -> advance (writeValue destination (readValue destination machine - 1) machine)
    Jump condition offset
        | readValue condition machine /= 0 -> machine{counter = machine.counter + readValue offset machine}
        | otherwise -> advance machine
    Toggle offset -> advance machine{program = Map.adjust toggle (machine.counter + readValue offset machine) machine.program}

advance :: Machine -> Machine
advance machine = machine{counter = machine.counter + 1}

toggle :: Instruction -> Instruction
toggle = \case
    Increment value -> Decrement value
    Decrement value -> Increment value
    Toggle value -> Increment value
    Jump source destination -> Copy source destination
    Copy source destination -> Jump source destination

readValue :: Value -> Machine -> Integer
readValue value machine = case value of
    Literal number -> number
    RegisterValue register -> Map.findWithDefault 0 register machine.registers

writeValue :: Value -> Integer -> Machine -> Machine
writeValue destination number machine = case destination of
    Literal _ -> machine
    RegisterValue register -> machine{registers = Map.insert register number machine.registers}
