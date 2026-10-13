{- |
approach: Count viable pairs; BFS the vacant and goal positions when checked
capacity bounds prove swaps are the only legal moves, otherwise BFS full loads.
cost: O(n^2 log n) time and O(n^2) memory for swaps; full search exponential
in node count. One parsing pass, one cached adjacency pass, then search traversals.
trade: Checked swap states use compact integer keys and cached adjacency;
the proved reduction handles large grids without guessing a route;
unreachable or missing goal data uses the documented part-2 placeholder 0.
-}
module AoC.Y2016.Day22.Primary (solver) where

import AoC.Grid (adjacent, indexedOrthogonal, orthogonal)
import AoC.Search (shortestDistance, shortestIntDistance)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day22.Input (Node (available, capacity, position, used), parseInput)
import Data.IntMap.Strict qualified as IntMap
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Position = (Int, Int)

type Loads = Map Position Int

solver :: Solver
solver text = do
    nodes <- parseInput text
    pure (Answers (show (viablePairs nodes)) (Just (show (fromMaybe 0 (fewestMoves nodes)))))

viablePairs :: [Node] -> Int
viablePairs nodes =
    sum
        [ 1
        | source <- nodes
        , target <- nodes
        , source.position /= target.position
        , source.used > 0
        , source.used <= target.available
        ]

fewestMoves :: [Node] -> Maybe Int
fewestMoves nodes = do
    (_, goal) <- bounds [node.position | node <- nodes, snd node.position == 0]
    guard (any (\node -> node.position == goal && node.used > 0) nodes)
    guard (any ((== (0, 0)) . (.position)) nodes)
    let fullSearch = shortestDistance (loadMoves capacities) ((== (0, 0)) . snd) (loads, goal)
    case swapModel nodes of
        Just (vacant, walkable)
            | Set.member goal walkable ->
                compactSearch walkable vacant goal
            | otherwise -> fullSearch
        Nothing -> fullSearch
  where
    capacities = Map.fromList [(node.position, node.capacity) | node <- nodes]
    loads = Map.fromList [(node.position, node.used) | node <- nodes]

compactSearch :: Set Position -> Position -> Position -> Maybe Int
compactSearch walkable vacant goal
    | toInteger count * toInteger count > toInteger (maxBound :: Int) = shortestDistance (swapMoves walkable) ((== (0, 0)) . snd) (vacant, goal)
    | otherwise = do
        emptyIndex <- Map.lookup vacant indices
        goalIndex <- Map.lookup goal indices
        target <- Map.lookup (0, 0) indices
        shortestIntDistance moves ((== target) . (`mod` count)) (emptyIndex * count + goalIndex)
  where
    count = Set.size walkable
    (indices, edges) = indexedOrthogonal walkable
    moves encoded = [source * count + if source == goalIndex then emptyIndex else goalIndex | source <- IntMap.findWithDefault [] emptyIndex edges]
      where
        (emptyIndex, goalIndex) = encoded `divMod` count

swapModel :: [Node] -> Maybe (Position, Set Position)
swapModel nodes = do
    (smallestCapacity, _) <- bounds (map (.capacity) nodes)
    let (mobile, walls) = partition ((<= smallestCapacity) . (.used)) nodes
    vacant <- case filter ((== 0) . (.used)) mobile of
        [node] -> Just node.position
        [] -> Nothing
        _ : _ : _ -> Nothing
    (lowCapacity, highCapacity) <- bounds (map (.capacity) mobile)
    (lowPayload, _) <- bounds [node.used | node <- mobile, node.used > 0]
    guard (all ((<= lowCapacity) . (.used)) mobile)
    -- No two payloads can merge; walls can neither receive nor send data.
    guard (unmergeable highCapacity mobile)
    guard (all (\wall -> wall.used > highCapacity && wall.available < lowPayload) walls)
    guard (and [a.used > b.available | a <- walls, b <- walls, adjacent a.position b.position])
    pure (vacant, Set.fromList (map (.position) mobile))

unmergeable :: Int -> [Node] -> Bool
unmergeable capacity nodes = case sort [node.used | node <- nodes, node.used > 0] of
    firstLoad : secondLoad : _ -> firstLoad + secondLoad > capacity
    [] -> True
    [_] -> True

swapMoves :: Set Position -> (Position, Position) -> [(Position, Position)]
swapMoves walkable (vacant, goal) =
    [(source, if source == goal then vacant else goal) | source <- orthogonal vacant, Set.member source walkable]

loadMoves :: Map Position Int -> (Loads, Position) -> [(Loads, Position)]
loadMoves capacities (loads, goal) =
    [ (Map.insert source 0 (Map.insert target (amount + occupied) loads), if source == goal then target else goal)
    | (source, amount) <- Map.toList loads
    , amount > 0
    , target <- orthogonal source
    , Just occupied <- [Map.lookup target loads]
    , Just capacity <- [Map.lookup target capacities]
    , amount <= capacity - occupied
    ]

bounds :: (Ord a) => [a] -> Maybe (a, a)
bounds = viaNonEmpty (\(firstValue :| remaining) -> foldl' widen (firstValue, firstValue) remaining)
  where
    widen :: (Ord a) => (a, a) -> a -> (a, a)
    widen (low, high) value = (min low value, max high value)
