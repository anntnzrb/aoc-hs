{- |
approach: Count ordered distinct node pairs whose nonempty payload fits.
cost: O(n^2) time, O(n) memory; one parsing pass and pair traversal.
trade: Direct comparison keeps the viability rule explicit.
-}
module AoC.Y2016.Day22.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day22.Input (Node (available, position, used), parseInput)

solver :: Solver
solver text = do
    nodes <- parseInput text
    pure (Answers (show (viablePairs nodes)) (Just "0"))

viablePairs :: [Node] -> Int
viablePairs nodes =
    sum
        [ 1
        | source <- nodes
        , target <- nodes
        , source.position /= target.position
        , source.used > 0
        , source.used <= target.available
        ]
