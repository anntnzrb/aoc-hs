{- | Breadth-first search in an implicit directed graph with unit-cost edges.
The successor function defines valid states and edges; the origin is always
included at distance zero. Each vertex appears once, at its minimum edge count,
regardless of duplicate edges or cycles. Layers are ordered by distance and
contain sets, with no ordering promised within a layer. An exhausted frontier
ends the lazy layer stream; infinite graphs may produce infinite streams.

'shortestDistance' stops at the first layer containing a matching vertex,
returning Nothing only when the reachable component is exhausted. 'distances'
traverses the whole reachable component and requires it to be finite. All
successor lists must be finite; distances use Int and must fit its range.
-}
module AoC.Search (breadthFirstLayers, shortestDistance, distances) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

breadthFirstLayers :: (Ord vertex) => (vertex -> [vertex]) -> vertex -> [(Int, Set vertex)]
breadthFirstLayers successors origin = unfoldr advance (0, one origin, one origin)
  where
    advance (distance, frontier, visited)
        | Set.null frontier = Nothing
        | otherwise = Just ((distance, frontier), (distance + 1, next, Set.union visited next))
      where
        next =
            frontier
                & Set.foldl' (\reached vertex -> foldl' discover reached (successors vertex)) Set.empty

        discover reached vertex
            | Set.member vertex visited = reached
            | otherwise = Set.insert vertex reached

shortestDistance :: (Ord vertex) => (vertex -> [vertex]) -> (vertex -> Bool) -> vertex -> Maybe Int
shortestDistance successors finished origin = breadthFirstLayers successors origin & find (any finished . snd) <&> fst

distances :: (Ord vertex) => (vertex -> [vertex]) -> vertex -> Map vertex Int
distances successors origin = breadthFirstLayers successors origin & foldl' addLayer Map.empty
  where
    addLayer :: (Ord vertex) => Map vertex Int -> (Int, Set vertex) -> Map vertex Int
    addLayer known (distance, frontier) = Set.foldl' (\reached vertex -> Map.insert vertex distance reached) known frontier
