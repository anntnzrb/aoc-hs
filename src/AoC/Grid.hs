{- | Unit-step adjacency on an integer lattice. Coordinates are an ordered
pair of axes: callers choose (x, y) or (row, column) consistently. Neighbours
are ordered first-axis negative, first-axis positive, second-axis negative,
second-axis positive. There are no diagonals, bounds, walls or wrapping rules.
'adjacent' means exactly one unit apart on one axis, never the same point;
its distance calculation is exact for every integral coordinate type.
For 'orthogonal', use a type that represents the needed unit offsets.
-}
module AoC.Grid (orthogonal, adjacent) where

orthogonal :: (Integral coordinate) => (coordinate, coordinate) -> [(coordinate, coordinate)]
orthogonal (firstAxis, secondAxis) = [(firstAxis - 1, secondAxis), (firstAxis + 1, secondAxis), (firstAxis, secondAxis - 1), (firstAxis, secondAxis + 1)]

adjacent :: (Integral coordinate) => (coordinate, coordinate) -> (coordinate, coordinate) -> Bool
adjacent (x, y) (otherX, otherY) = abs (toInteger x - toInteger otherX) + abs (toInteger y - toInteger otherY) == 1
