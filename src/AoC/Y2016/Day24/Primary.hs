{- |
approach: Breadth-first distances, subset route DP, and optional return to start.
cost: O(k V log V + 2^k k^3) time, O(V + 2^k k) memory; four parsing passes, k grid searches.
trade: Compresses corridor walks into exact distances; stores subset endpoints.
-}
module AoC.Y2016.Day24.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day24.Input (Input (locations, passages), parseInput)
import Data.Bits (setBit, testBit)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Distances = Map Int (Map Int Int)

type Routes = Map (Int, Int) Int

solver :: Solver
solver text = do
    input <- parseInput text
    let distances = locationDistances input
        routes = foldl' (\states _ -> extend distances states) (one ((setBit 0 0, 0), 0)) [1 .. Map.size input.locations - 1]
    answer <- viaNonEmpty (\(value :| values) -> foldl' min value values) (Map.elems routes) & maybeToRight "Unreachable numbered location"
    returning <- viaNonEmpty (\(value :| values) -> foldl' min value values) [cost + distance | ((_, endpoint), cost) <- Map.toList routes, Just distance <- [Map.lookup endpoint distances >>= Map.lookup 0]] & maybeToRight "Cannot return to starting location"
    pure (Answers (show answer) (Just (show returning)))

locationDistances :: Input -> Distances
locationDistances input = Map.fromList [(source, Map.fromList [(target, distance) | (target, position) <- Map.toList input.locations, Just distance <- [Map.lookup position reached]]) | (source, origin) <- Map.toList input.locations, let reached = breadthFirst input.passages origin]

breadthFirst :: Set (Int, Int) -> (Int, Int) -> Map (Int, Int) Int
breadthFirst walkable origin = go 0 (one origin) Map.empty
  where
    go :: Int -> Set (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
    go distance frontier visited
        | Set.null frontier = visited
        | otherwise = go (distance + 1) next reached
      where
        reached = Set.foldl' (\known position -> Map.insert position distance known) visited frontier
        next =
            frontier
                & Set.toList
                & concatMap neighbors
                & Set.fromList
                & (`Set.intersection` walkable)
                & Set.filter (`Map.notMember` reached)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (row, column) = [(row - 1, column), (row + 1, column), (row, column - 1), (row, column + 1)]

extend :: Distances -> Routes -> Routes
extend distances states = Map.fromListWith min [((setBit visited target, target), cost + distance) | ((visited, source), cost) <- Map.toList states, (target, distance) <- Map.toList (Map.findWithDefault Map.empty source distances), not (testBit visited target)]
