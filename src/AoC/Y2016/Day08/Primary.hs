{- |
approach: Unfinished scaffold; derive the algorithm from the statement.
cost: No input traversal or solver work yet.
trade: Placeholder until the statement-derived implementation is written.
-}
module AoC.Y2016.Day08.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day08.Input (Input (Input), parseInput)

solver :: Solver
solver text = do
    Input _ <- parseInput text
    pure (Answers "0" (Just "0"))
