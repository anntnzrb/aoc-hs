{- |
approach: Try positive seeds in order, interpreting until a mismatch, halt or repeated complete state proves a productive alternating cycle.
cost: O(n + tn log s) time and O(sn) memory for n instructions, t total steps and s distinct states in the largest trial; one parse pass and one indexing pass, with integer arithmetic costs additional.
trade: Exact cycle detection proves endless output rather than trusting a finite prefix; storing the live program also preserves self-modifying semantics.
-}
module AoC.Y2016.Day25.Primary (solver) where

import AoC.Assembunny (Instruction (Output), Machine (registers), Register (A, B, C, D), current, initialise, readValue, step)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day25.Input (Input (Input), parseInput)
import Data.Map.Strict qualified as Map

data Signal = Signal
    { machine :: Machine
    , expected :: Integer
    }
    deriving stock (Eq, Ord)

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    let machine = initialise instructions []
    seed <- [1 ..] & find (clock machine) & maybeToRight "No positive clock seed"
    pure (Answers (show seed) Nothing)

clock :: Machine -> Integer -> Bool
clock machine seed = verify mempty 0 (Signal machine{registers = Map.fromList [(A, seed), (B, 0), (C, 0), (D, 0)]} 0)

-- Include the next expected bit in the key; a silent cycle emits no new bits.
verify :: Map Signal Integer -> Integer -> Signal -> Bool
verify seen emitted signal = case Map.lookup signal seen of
    Just previous -> emitted > previous
    Nothing -> case current signal.machine of
        Nothing -> False
        Just (Output value)
            | readValue value signal.machine /= signal.expected -> False
            | otherwise -> continue 1 (1 - signal.expected)
        Just _ -> continue 0 signal.expected
  where
    continue :: Integer -> Integer -> Bool
    continue count expected = verify (Map.insert signal emitted seen) (emitted + count) (Signal (step signal.machine) expected)
