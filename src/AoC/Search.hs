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
The Int variants have identical semantics for graphs whose vertices are Ints;
callers encoding other states must use an injective, overflow-free encoding.
-}
module AoC.Search (breadthFirstLayers, shortestDistance, distances, shortestIntDistance, intDistances) where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
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

shortestIntDistance :: (Int -> [Int]) -> (Int -> Bool) -> Int -> Maybe Int
shortestIntDistance successors finished origin = intLayers successors origin & find (IntSet.foldr (\vertex rest -> finished vertex || rest) False . snd) <&> fst

intDistances :: (Int -> [Int]) -> Int -> IntMap Int
intDistances successors origin = intLayers successors origin & foldl' addLayer IntMap.empty
  where
    addLayer :: IntMap Int -> (Int, IntSet) -> IntMap Int
    addLayer known (distance, frontier) = IntSet.foldl' (\reached vertex -> IntMap.insert vertex distance reached) known frontier

intLayers :: (Int -> [Int]) -> Int -> [(Int, IntSet)]
intLayers successors origin = unfoldr advance (0, one origin, one origin)
  where
    advance (distance, frontier, visited)
        | IntSet.null frontier = Nothing
        | otherwise = Just ((distance, frontier), (distance + 1, next, IntSet.union visited next))
      where
        next = IntSet.foldl' (\reached vertex -> foldl' discover reached (successors vertex)) IntSet.empty frontier
        discover reached vertex
            | IntSet.member vertex visited = reached
            | otherwise = IntSet.insert vertex reached
