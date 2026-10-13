{- |
approach: Fold instructions over lit coordinates; rotations wrap their selected line.
cost: O(n * s log s) time, O(n + s) memory; parse and fold passes, s <= 300.
trade: A set makes overlaps idempotent and simultaneous rotations explicit.
-}
module AoC.Y2016.Day08.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day08.Input (Instruction (Rect, RotateColumn, RotateRow), parseInput)
import Data.Set qualified as Set

type Screen = Set (Int, Int)

solver :: Solver
solver text = do
    instructions <- parseInput text
    let screen = foldl' apply Set.empty instructions
    pure (Answers (show (Set.size screen)) (Just "0"))

apply :: Screen -> Instruction -> Screen
apply screen = \case
    Rect width height -> screen `Set.union` Set.fromList [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    RotateRow row amount -> screen & Set.map (\(x, y) -> (if y == row then (x + amount) `mod` 50 else x, y))
    RotateColumn column amount -> screen & Set.map (\(x, y) -> (x, if x == column then (y + amount) `mod` 6 else y))
