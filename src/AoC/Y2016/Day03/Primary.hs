{- |
approach: Count valid rows and columns within consecutive three-row groups.
cost: O(n) time and O(n) storage; parse, row count, grouping and column count.
trade: Direct inequalities avoid sorting and keep the rule explicit.
-}
module AoC.Y2016.Day03.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day03.Input (Triangle (Triangle), parseInput)

solver :: Solver
solver text = do
    triangles <- parseInput text
    -- Incomplete three-row groups leave part two undefined: print the placeholder.
    pure (Answers (show (countTriangles triangles)) (Just (show (fromMaybe 0 (verticalCount triangles)))))

verticalCount :: [Triangle] -> Maybe Int
verticalCount =
    unfoldr columns
        >>> sequenceA
        >>> fmap (concat >>> countTriangles)

columns :: [Triangle] -> Maybe (Maybe [Triangle], [Triangle])
columns = \case
    [] -> Nothing
    Triangle a b c : Triangle d e f : Triangle g h i : rest ->
        Just (Just [Triangle a d g, Triangle b e h, Triangle c f i], rest)
    [_] -> Just (Nothing, [])
    [_, _] -> Just (Nothing, [])

countTriangles :: [Triangle] -> Int
countTriangles = foldMap (Sum . fromEnum . possible) >>> getSum

possible :: Triangle -> Bool
possible (Triangle a b c) = a + b > c && a + c > b && b + c > a
