{- |
approach: Eliminate alternating seats in rounds; powers of two reset the winner.
cost: O(log n) arithmetic steps, O(1) live integers; one input pass.
trade: Round arithmetic avoids storing the circle; part two is unfinished.
-}
module AoC.Y2016.Day19.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day19.Input (Input (elfCount), parseInput)

solver :: Solver
solver text = do
    input <- parseInput text
    pure (Answers (show (leftWinner input.elfCount)) (Just "0"))

leftWinner :: Integer -> Integer
leftWinner count = 2 * (count - power) + 1
  where
    -- Removing every second seat gives J(2m)=2J(m)-1, J(2m+1)=2J(m)+1.
    power = iterate (* 2) 1 & takeWhile (<= count) & foldl' max 1
