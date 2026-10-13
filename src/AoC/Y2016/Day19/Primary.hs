{- |
approach: Alternate-seat rounds for part one; undo opposite removals for part two.
cost: O(n) arithmetic steps, O(1) live integers; one input pass.
trade: Survivor recurrences avoid storing or rotating the circle.
-}
module AoC.Y2016.Day19.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day19.Input (Input (elfCount), parseInput)

solver :: Solver
solver text = do
    input <- parseInput text
    pure (Answers (show (leftWinner input.elfCount)) (Just (show (acrossWinner input.elfCount))))

leftWinner :: Integer -> Integer
leftWinner count = 2 * (count - power) + 1
  where
    -- Removing every second seat gives J(2m)=2J(m)-1, J(2m+1)=2J(m)+1.
    power = iterate (* 2) 1 & takeWhile (<= count) & foldl' max 1

acrossWinner :: Integer -> Integer
acrossWinner count = foldl' restore 1 [2 .. count]
  where
    -- Undo rotation to the next elf, then reinsert the removed opposite seat.
    restore :: Integer -> Integer -> Integer
    restore winner size
        | winner < size `div` 2 = winner + 1
        | winner == size - 1 = 1
        | otherwise = winner + 2
