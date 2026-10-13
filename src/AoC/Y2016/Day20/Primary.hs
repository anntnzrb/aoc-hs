{- |
approach: Sort blocked ranges; find the first gap and sum all uncovered gaps.
cost: O(n log n) time, O(n) memory; one parse, sorting, two range folds.
trade: Range arithmetic avoids enumerating the entire address space.
-}
module AoC.Y2016.Day20.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day20.Input (addressCount, bounds, parseInput)

{- | Return the lowest allowed unsigned 32-bit address and total allowed addresses; use
0 for the first answer when all are blocked. Input grammar errors are returned as
text.
-}
solver :: Solver
solver text = do
    ranges <- parseInput text
    let sorted = ranges <&> bounds & sort
        lowest = sorted & foldl' advance 0
    -- A fully blocked address space has no lowest allowed address.
    answers (show (if lowest < addressCount then lowest else 0)) (Just (show (allowedCount sorted)))

advance :: Integer -> (Integer, Integer) -> Integer
advance candidate (lower, upper)
    | lower <= candidate = max candidate (upper + 1)
    | otherwise = candidate

allowedCount :: [(Integer, Integer)] -> Integer
allowedCount ranges = gaps + addressCount - next
  where
    (next, gaps) = foldl' countGap (0, 0) ranges

countGap :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
countGap (next, gaps) (lower, upper) =
    (max next (upper + 1), gaps + max 0 (lower - next))
