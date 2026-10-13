{- |
approach: Try positive seeds in order, collapsing verified terminating arithmetic loops until a mismatch, halt or repeated complete state proves a productive alternating cycle.
cost: O(n + tn log s) time and O(sn) memory for n instructions, t total interpreted or collapsed steps and s distinct states in the largest trial; one parse pass and one indexing pass, with integer arithmetic costs additional.
trade: Exact cycle detection still includes the live program and expected bit; skipping only finite output-free loops preserves the proof while avoiding repeated arithmetic instructions.
-}
module AoC.Y2016.Day25.Primary (solver) where

import AoC.Assembunny (Instruction (Output), Machine (registers), Register (A, B, C, D), acceleratedStep, current, initialise, readValue)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day25.Input (Input (Input), parseInput)
import Data.Map.Strict qualified as Map

data Bit = Zero | One
    deriving stock (Eq, Ord)

data Signal = Signal
    { machine :: Machine
    , expected :: Bit
    }
    deriving stock (Eq, Ord)

{- | Return the smallest positive register-a seed proving an infinite alternating 0/1
clock; no computational second answer. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input instructions <- parseInput text
    let machine = initialise instructions []
    seed <- [1 ..] & find (clock machine) & maybeToRight "No positive clock seed"
    answers (show seed) Nothing

clock :: Machine -> Integer -> Bool
clock machine seed = verify mempty 0 (Signal machine{registers = Map.fromList [(A, seed), (B, 0), (C, 0), (D, 0)]} Zero)

flipBit :: Bit -> Bit
flipBit = \case
    Zero -> One
    One -> Zero

bitValue :: Bit -> Integer
bitValue = \case
    Zero -> 0
    One -> 1

-- Include the next expected bit in the key; a silent cycle emits no new bits.
-- Every collapsed edge is finite and output-free, so repetition still proves
-- repetition of the complete original machine with the same output phase.
verify :: Map Signal Integer -> Integer -> Signal -> Bool
verify seen emitted signal = case Map.lookup signal seen of
    Just previous -> emitted > previous
    Nothing -> case current signal.machine of
        Nothing -> False
        Just (Output value)
            | readValue value signal.machine /= bitValue signal.expected -> False
            | otherwise -> continue 1 (flipBit signal.expected)
        Just _ -> continue 0 signal.expected
  where
    continue :: Integer -> Bit -> Bool
    continue count expected = verify (Map.insert signal emitted seen) (emitted + count) (Signal (acceleratedStep signal.machine) expected)
