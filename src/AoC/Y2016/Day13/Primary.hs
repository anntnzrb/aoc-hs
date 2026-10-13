{- |
approach: Expand open coordinates in breadth-first layers to find the target.
cost: O(V log V) time and O(V) memory for visited coordinates; one input pass.
trade: Set frontiers deduplicate paths without storing the infinite maze.
-}
module AoC.Y2016.Day13.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day13.Input (Input (Input), parseInput)
import Data.Bits (popCount)
import Data.Set qualified as Set

type Position = (Integer, Integer)

solver :: Solver
solver text = do
    Input favorite <- parseInput text
    pure (Answers (show (shortest favorite (31, 39))) (Just "0"))

shortest :: Natural -> Position -> Int
shortest favorite target
    -- A wall or an exhausted component has no route: use the undefined placeholder.
    | not (isOpen favorite target) = 0
    | otherwise = layers favorite & find (Set.member target . snd) & maybe 0 fst

layers :: Natural -> [(Int, Set Position)]
layers favorite = unfoldr advance (0, start, start)
  where
    start = Set.fromList [(1, 1) | isOpen favorite (1, 1)]
    advance (distance, frontier, visited)
        | Set.null frontier = Nothing
        | otherwise = Just ((distance, frontier), (distance + 1, next, Set.union visited next))
      where
        next = frontier & Set.toList & concatMap (filter (isOpen favorite) . neighbors) & Set.fromList & (`Set.difference` visited)

neighbors :: Position -> [Position]
neighbors (x, y) = [(a, b) | (a, b) <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)], a >= 0, b >= 0]

isOpen :: Natural -> Position -> Bool
isOpen favorite (x, y) = even (popCount (x * x + 3 * x + 2 * x * y + y + y * y + toInteger favorite))
