{- |
approach: Try positive seeds in order, interpreting until a mismatch, halt or repeated complete state proves a productive alternating cycle.
cost: O(n + tn log s) time and O(sn) memory for n instructions, t total steps and s distinct states in the largest trial; one parse pass and one indexing pass, with integer arithmetic costs additional.
trade: Exact cycle detection proves endless output rather than trusting a finite prefix; storing the live program also preserves self-modifying semantics.
-}
module AoC.Y2016.Day25.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day25.Input (
    Input (Input),
    Instruction (Copy, Decrement, Increment, Jump, Output, Toggle),
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    parseInput,
 )
import Data.Map.Strict qualified as Map

data Machine = Machine
    { counter :: Integer
    , registers :: Map Register Integer
    , program :: Map Integer Instruction
    , expected :: Integer
    }
    deriving stock (Eq, Ord)

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    let program = Map.fromDistinctAscList (zip [0 ..] instructions)
    seed <- [1 ..] & find (clock program) & maybeToRight "No positive clock seed"
    pure (Answers (show seed) (Just "0"))

clock :: Map Integer Instruction -> Integer -> Bool
clock program seed = verify mempty 0 (Machine 0 (Map.fromList [(A, seed), (B, 0), (C, 0), (D, 0)]) program 0)

-- Include the next expected bit in the key; a silent cycle emits no new bits.
verify :: Map Machine Integer -> Integer -> Machine -> Bool
verify seen emitted machine = case Map.lookup machine seen of
    Just previous -> emitted > previous
    Nothing -> case Map.lookup machine.counter machine.program of
        Nothing -> False
        Just (Output value)
            | readValue value machine /= machine.expected -> False
        Just instruction -> verify (Map.insert machine emitted seen) (emitted + outputCount instruction) (apply machine instruction)

outputCount :: Instruction -> Integer
outputCount = \case
    Output _ -> 1
    Copy{} -> 0
    Increment{} -> 0
    Decrement{} -> 0
    Jump{} -> 0
    Toggle{} -> 0

apply :: Machine -> Instruction -> Machine
apply machine = \case
    Copy source destination -> advance (writeValue destination (readValue source machine) machine)
    Increment destination -> advance (writeValue destination (readValue destination machine + 1) machine)
    Decrement destination -> advance (writeValue destination (readValue destination machine - 1) machine)
    Jump condition offset
        | readValue condition machine /= 0 -> machine{counter = machine.counter + readValue offset machine}
        | otherwise -> advance machine
    Toggle offset -> advance machine{program = Map.adjust toggle (machine.counter + readValue offset machine) machine.program}
    Output _ -> advance machine{expected = 1 - machine.expected}

advance :: Machine -> Machine
advance machine = machine{counter = machine.counter + 1}

toggle :: Instruction -> Instruction
toggle = \case
    Increment value -> Decrement value
    Decrement value -> Increment value
    Toggle value -> Increment value
    Output value -> Increment value
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
