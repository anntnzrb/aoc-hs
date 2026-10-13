{- |
approach: Breadth-first distances, subset route DP, and optional return to start.
cost: O(k V log V + 2^k k^3) time, O(V + 2^k k) memory; four parsing passes, one adjacency pass and k integer graph searches.
trade: Cache corridor adjacency once and use dense integer vertex keys for every exact distance search; subset endpoints retain the same route DP.
-}
module AoC.Y2016.Day24.Primary (solver) where

import AoC.Grid (indexedOrthogonal)
import AoC.Search qualified as Search
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day24.Input (Input (locations, passages), parseInput)
import Data.Bits (setBit, testBit)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map

type Distances = Map Int (Map Int Int)

type Routes = Map (Int, Int) Int

solver :: Solver
solver text = do
    input <- parseInput text
    let distances = locationDistances input
        routes = foldl' (\states _ -> extend distances states) (one ((setBit 0 0, 0), 0)) [1 .. Map.size input.locations - 1]
    answer <- viaNonEmpty (\(value :| values) -> foldl' min value values) (Map.elems routes) & maybeToRight "Unreachable numbered location"
    returning <- viaNonEmpty (\(value :| values) -> foldl' min value values) [cost + distance | ((_, endpoint), cost) <- Map.toList routes, Just distance <- [Map.lookup endpoint distances >>= Map.lookup 0]] & maybeToRight "Cannot return to starting location"
    answers (show answer) (Just (show returning))

locationDistances :: Input -> Distances
locationDistances input = Map.map fromLocation numbered
  where
    (indices, edges) = indexedOrthogonal input.passages
    numbered = Map.mapMaybe (`Map.lookup` indices) input.locations
    fromLocation origin = Map.mapMaybe (`IntMap.lookup` reached) numbered
      where
        reached = Search.intDistances (\index -> IntMap.findWithDefault [] index edges) origin

extend :: Distances -> Routes -> Routes
extend distances states = Map.fromListWith min [((setBit visited target, target), cost + distance) | ((visited, source), cost) <- Map.toList states, (target, distance) <- Map.toList (Map.findWithDefault Map.empty source distances), not (testBit visited target)]
