{- | Unit-step adjacency on an integer lattice. Coordinates are an ordered
pair of axes: callers choose (x, y) or (row, column) consistently. Neighbours
are ordered first-axis negative, first-axis positive, second-axis negative,
second-axis positive. There are no diagonals, bounds, walls or wrapping rules.
'adjacent' means exactly one unit apart on one axis, never the same point;
its distance calculation is exact for every integral coordinate type.
For 'orthogonal', use a type that represents the needed unit offsets.
'indexedOrthogonal' numbers a finite set densely from zero in ascending set
order and returns each index's neighbours within that set, in 'orthogonal'
order. Empty sets yield two empty maps. The set size must fit in Int and its
coordinate type must represent all unit offsets used by 'orthogonal'.
-}
module AoC.Grid (orthogonal, adjacent, indexedOrthogonal) where

import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

orthogonal :: (Integral coordinate) => (coordinate, coordinate) -> [(coordinate, coordinate)]
orthogonal (firstAxis, secondAxis) = [(firstAxis - 1, secondAxis), (firstAxis + 1, secondAxis), (firstAxis, secondAxis - 1), (firstAxis, secondAxis + 1)]

adjacent :: (Integral coordinate) => (coordinate, coordinate) -> (coordinate, coordinate) -> Bool
adjacent (x, y) (otherX, otherY) = abs (toInteger x - toInteger otherX) + abs (toInteger y - toInteger otherY) == 1

indexedOrthogonal :: (Integral coordinate) => Set (coordinate, coordinate) -> (Map (coordinate, coordinate) Int, IntMap [Int])
indexedOrthogonal positions = (indices, edges)
  where
    indices = Map.fromList (zip (Set.toAscList positions) [0 ..])
    edges = IntMap.fromList [(index, mapMaybe (`Map.lookup` indices) (orthogonal position)) | (position, index) <- Map.toAscList indices]
