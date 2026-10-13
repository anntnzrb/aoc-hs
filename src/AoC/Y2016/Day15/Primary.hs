{- |
approach: Merge disc alignment schedules, then extend the same schedule with the added disc.
cost: O(sum of disc sizes divided by their gcd with the prior period) time,
      O(n) input memory and three passes (parse, merge, bottom lookup), using arbitrary integers.
trade: Period stepping skips rejected times; incompatible part 2 prints the undefined-answer placeholder 0.
-}
module AoC.Y2016.Day15.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day15.Input (Disc (initial, number, positions), disc, parseInput)

solver :: Solver
solver text = do
    discs <- parseInput text
    schedule <- foldlM align (0, 1) discs
    extra <- disc (1 + foldl' (\bottom item -> max bottom item.number) 0 discs) 11 0
    let part2 = align schedule extra & either (const "0") (show . fst)
    pure (Answers (show (fst schedule)) (Just part2))

align :: (Integer, Integer) -> Disc -> Either Text (Integer, Integer)
align (time, period) item =
    [time + period * offset | offset <- [0 .. item.positions `div` common - 1]]
        & find (\candidate -> (candidate + item.number + item.initial) `mod` item.positions == 0)
        & maybe (Left "Disc slots never align") (\candidate -> Right (candidate, lcm period item.positions))
  where
    common = gcd period item.positions
