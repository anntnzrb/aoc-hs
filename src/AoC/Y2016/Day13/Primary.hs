{- |
approach: Breadth-first layers find the target and count rooms within 50 steps.
cost: O(V log V) time and O(V) memory for visited coordinates; one input pass and one shared graph traversal.
trade: Shared lazy layers reuse the target search for the 50-step room count, retaining only the prefix needed by the second answer.
-}
module AoC.Y2016.Day13.Primary (solver) where

import AoC.Grid (orthogonal)
import AoC.Search (breadthFirstLayers)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day13.Input (Input (Input), parseInput)
import Data.Bits (popCount)
import Data.Set qualified as Set

type Position = (Integer, Integer)

{- | Return shortest steps to (31,39) and the count reachable within 50 steps; use 0 if
the target is unreachable. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input favorite <- parseInput text
    let rooms = layers favorite
        -- A wall or an exhausted component has no route: use the undefined placeholder.
        steps
            | isOpen favorite (31, 39) =
                rooms
                    & find (Set.member (31, 39) . snd)
                    & maybe 0 fst
            | otherwise = 0
        nearby =
            rooms
                & take 51
                & foldMap (Sum . Set.size . snd)
                & getSum
    answers (show steps) (Just (show nearby))

layers :: Natural -> [(Int, Set Position)]
layers favorite
    | isOpen favorite (1, 1) = breadthFirstLayers (neighbors >>> filter (isOpen favorite)) (1, 1)
    | otherwise = []

neighbors :: Position -> [Position]
neighbors position = [(x, y) | (x, y) <- orthogonal position, x >= 0, y >= 0]

isOpen :: Natural -> Position -> Bool
isOpen favorite (x, y) = even (popCount (x * x + 3 * x + 2 * x * y + y + y * y + toInteger favorite))
