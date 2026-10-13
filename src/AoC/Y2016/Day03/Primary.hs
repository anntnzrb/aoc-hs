{- |
approach: Count rows satisfying all three strict triangle inequalities.
cost: O(n) time and O(n) parsed storage; one parse and one row traversal.
trade: Direct inequalities avoid sorting and keep the rule explicit.
-}
module AoC.Y2016.Day03.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day03.Input (Triangle (Triangle), parseInput)

solver :: Solver
solver text = do
    triangles <- parseInput text
    pure (Answers (show (countTriangles triangles)) (Just "0"))

countTriangles :: [Triangle] -> Int
countTriangles = foldl' (\count sides -> count + fromEnum (possible sides)) 0

possible :: Triangle -> Bool
possible (Triangle a b c) = a + b > c && a + c > b && b + c > a
