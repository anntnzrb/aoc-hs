{- |
approach: Sort blocked ranges and advance the lowest candidate through coverage.
cost: O(n log n) time, O(n) memory; one parse, sorting, one range fold.
trade: Range arithmetic avoids enumerating the entire address space.
-}
module AoC.Y2016.Day20.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day20.Input (addressCount, bounds, parseInput)

solver :: Solver
solver text = do
    ranges <- parseInput text
    let lowest = ranges <&> bounds & sort & foldl' advance 0
    -- A fully blocked address space has no lowest allowed address.
    pure (Answers (show (if lowest < addressCount then lowest else 0)) (Just "0"))

advance :: Integer -> (Integer, Integer) -> Integer
advance candidate (lower, upper)
    | lower <= candidate = max candidate (upper + 1)
    | otherwise = candidate
