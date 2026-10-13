{- |
approach: Alternate-seat rounds for part one; solve the opposite-removal recurrence between powers of three for part two.
cost: O(log n) arithmetic steps, O(log n) bits of live integers; one input pass.
trade: Power boundaries replace one recurrence step per elf with three exact linear cases, without storing the circle.
-}
module AoC.Y2016.Day19.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day19.Input (Input (elfCount), parseInput)

solver :: Solver
solver text = do
    input <- parseInput text
    answers (show (leftWinner input.elfCount)) (Just (show (acrossWinner input.elfCount)))

leftWinner :: Integer -> Integer
leftWinner count = 2 * (count - power) + 1
  where
    -- Removing every second seat gives J(2m)=2J(m)-1, J(2m+1)=2J(m)+1.
    power =
        iterate (* 2) 1
            & takeWhile (<= count)
            & foldl' max 1

acrossWinner :: Integer -> Integer
acrossWinner count
    | count == power = power
    | count <= 2 * power = count - power
    | otherwise = 2 * count - 3 * power
  where
    -- Undoing rotation and reinserting the opposite seat increments the winner
    -- by one below half the new size, wraps size-1 to one, otherwise adds two.
    -- Starting at W(p)=p, this gives +1 until 2p, then +2 until W(3p)=3p.
    power =
        iterate (* 3) 1
            & takeWhile (<= count)
            & foldl' max 1
