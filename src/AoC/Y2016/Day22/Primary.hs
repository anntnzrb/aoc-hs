{- |
approach: Count viable pairs; BFS the vacant and goal positions when checked
capacity bounds prove swaps are the only legal moves, otherwise BFS full loads.
cost: O(n^2 log n) time and O(n^2) memory for swaps; full search exponential
in node count. One parsing pass, then bounded node and search traversals.
trade: A proved state reduction handles large grids without guessing a route;
unreachable or missing goal data uses the documented part-2 placeholder 0.
-}
module AoC.Y2016.Day22.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day22.Input (Node (available, capacity, position, used), parseInput)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
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
    let fullSearch = breadthFirst (loadMoves capacities) ((== (0, 0)) . snd) (loads, goal)
    case swapModel nodes of
        Just (vacant, walkable)
            | Set.member goal walkable ->
                breadthFirst (swapMoves walkable) ((== (0, 0)) . snd) (vacant, goal)
            | otherwise -> fullSearch
        Nothing -> fullSearch
  where
    capacities = Map.fromList [(node.position, node.capacity) | node <- nodes]
    loads = Map.fromList [(node.position, node.used) | node <- nodes]

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

neighbors :: Position -> [Position]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

adjacent :: Position -> Position -> Bool
adjacent (x, y) (otherX, otherY) = abs (x - otherX) + abs (y - otherY) == 1

swapMoves :: Set Position -> (Position, Position) -> [(Position, Position)]
swapMoves walkable (vacant, goal) =
    [(source, if source == goal then vacant else goal) | source <- neighbors vacant, Set.member source walkable]

loadMoves :: Map Position Int -> (Loads, Position) -> [(Loads, Position)]
loadMoves capacities (loads, goal) =
    [ (Map.insert source 0 (Map.insert target (amount + occupied) loads), if source == goal then target else goal)
    | (source, amount) <- Map.toList loads
    , amount > 0
    , target <- neighbors source
    , Just occupied <- [Map.lookup target loads]
    , Just capacity <- [Map.lookup target capacities]
    , amount <= capacity - occupied
    ]

breadthFirst :: forall state. (Ord state) => (state -> [state]) -> (state -> Bool) -> state -> Maybe Int
breadthFirst next finished initial = search (one initial) (one (initial, 0))
  where
    search :: Set state -> Seq.Seq (state, Int) -> Maybe Int
    search seen queue = case Seq.viewl queue of
        Seq.EmptyL -> Nothing
        (current, distance) Seq.:< rest
            | finished current -> Just distance
            | otherwise ->
                let fresh = Set.fromList (next current) `Set.difference` seen
                    queue' = rest Seq.>< Seq.fromList [(successor, distance + 1) | successor <- Set.toList fresh]
                 in search (Set.union seen fresh) queue'

bounds :: (Ord a) => [a] -> Maybe (a, a)
bounds = viaNonEmpty (\(firstValue :| remaining) -> foldl' widen (firstValue, firstValue) remaining)
  where
    widen :: (Ord a) => (a, a) -> a -> (a, a)
    widen (low, high) value = (min low value, max high value)
