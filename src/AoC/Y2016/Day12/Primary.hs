{- |
approach: Interpret each instruction with four integer registers and a relative program counter.
cost: O(n + t log n) time, O(n) memory; one parse pass and one indexing pass for n instructions and t total executed steps across both runs.
trade: A direct interpreter preserves the instruction semantics without assuming a particular program; the two initial register states share the same parsed program.
-}
module AoC.Y2016.Day12.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day12.Input (
    Input (Input),
    Instruction (Copy, Decrement, Increment, Jump),
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    parseInput,
 )
import Data.Map.Strict qualified as Map

data Registers = Registers
    { a :: Integer
    , b :: Integer
    , c :: Integer
    , d :: Integer
    }

data Machine = Machine
    { counter :: Integer
    , registers :: Registers
    }

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    let program = Map.fromDistinctAscList (zip [0 ..] instructions)
    pure (Answers (show (execute program (Registers 0 0 0 0))) (Just (show (execute program (Registers 0 0 1 0)))))

execute :: Map Integer Instruction -> Registers -> Integer
execute program initial =
    Machine 0 initial
        & iterate (step program)
        & find (\machine -> Map.notMember machine.counter program)
        & maybe 0 (\machine -> machine.registers.a)

step :: Map Integer Instruction -> Machine -> Machine
step program machine = case Map.lookup machine.counter program of
    Nothing -> machine
    Just (Copy source destination) -> advance (writeRegister destination (readValue machine.registers source) machine.registers)
    Just (Increment destination) -> advance (writeRegister destination (readRegister destination machine.registers + 1) machine.registers)
    Just (Decrement destination) -> advance (writeRegister destination (readRegister destination machine.registers - 1) machine.registers)
    Just (Jump condition offset)
        | readValue machine.registers condition /= 0 -> machine{counter = machine.counter + offset}
        | otherwise -> advance machine.registers
  where
    advance :: Registers -> Machine
    advance = Machine (machine.counter + 1)

readValue :: Registers -> Value -> Integer
readValue registers = \case
    Literal value -> value
    RegisterValue register -> readRegister register registers

readRegister :: Register -> Registers -> Integer
readRegister register registers = case register of
    A -> registers.a
    B -> registers.b
    C -> registers.c
    D -> registers.d

writeRegister :: Register -> Integer -> Registers -> Registers
writeRegister register value registers = case register of
    A -> registers{a = value}
    B -> registers{b = value}
    C -> registers{c = value}
    D -> registers{d = value}
