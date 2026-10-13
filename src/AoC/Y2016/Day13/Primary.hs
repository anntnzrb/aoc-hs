{- |
approach: Breadth-first layers find the target and count rooms within 50 steps.
cost: O(V log V) time and O(V) memory for visited coordinates; one input pass.
trade: Set frontiers deduplicate paths; lazy layers stop each search on demand.
-}
module AoC.Y2016.Day13.Primary (solver) where

import AoC.Grid (orthogonal)
import AoC.Search (breadthFirstLayers)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day13.Input (Input (Input), parseInput)
import Data.Bits (popCount)
import Data.Set qualified as Set

type Position = (Integer, Integer)

solver :: Solver
solver text = do
    Input favorite <- parseInput text
    pure (Answers (show (shortest favorite (31, 39))) (Just (show (reachable favorite))))

shortest :: Natural -> Position -> Int
shortest favorite target
    -- A wall or an exhausted component has no route: use the undefined placeholder.
    | not (isOpen favorite target) = 0
    | otherwise = layers favorite & find (Set.member target . snd) & maybe 0 fst

layers :: Natural -> [(Int, Set Position)]
layers favorite
    | isOpen favorite (1, 1) = breadthFirstLayers (neighbors >>> filter (isOpen favorite)) (1, 1)
    | otherwise = []

neighbors :: Position -> [Position]
neighbors position = [(x, y) | (x, y) <- orthogonal position, x >= 0, y >= 0]

isOpen :: Natural -> Position -> Bool
isOpen favorite (x, y) = even (popCount (x * x + 3 * x + 2 * x * y + y + y * y + toInteger favorite))

reachable :: Natural -> Int
reachable favorite = layers favorite & take 51 & map (Set.size . snd) & sum
