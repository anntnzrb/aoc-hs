{- |
approach: Align each disc at release time plus its arrival second; merge periodic schedules.
cost: O(sum of disc sizes divided by their gcd with the prior period) time,
      O(n) input memory and two passes (parse and merge), using arbitrary integers.
trade: Period stepping skips all times rejected by earlier discs and detects incompatible slots.
-}
module AoC.Y2016.Day15.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day15.Input (Disc (initial, number, positions), parseInput)

solver :: Solver
solver text = do
    discs <- parseInput text
    time <- release discs
    pure (Answers (show time) (Just "0"))

release :: [Disc] -> Either Text Integer
release = foldlM align (0, 1) >>> fmap fst

align :: (Integer, Integer) -> Disc -> Either Text (Integer, Integer)
align (time, period) disc =
    [time + period * offset | offset <- [0 .. disc.positions `div` common - 1]]
        & find (\candidate -> (candidate + disc.number + disc.initial) `mod` disc.positions == 0)
        & maybe (Left "Disc slots never align") (\candidate -> Right (candidate, lcm period disc.positions))
  where
    common = gcd period disc.positions
