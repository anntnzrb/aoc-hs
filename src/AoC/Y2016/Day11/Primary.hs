{- |
approach: Breadth-first elevator search; sort generator/chip floor pairs to identify equivalent element renamings.
cost: O(V n^3 log(V n)) time and O(V n) memory for V reachable canonical states and n pairs; one parsing pass.
trade: Exact shortest paths with symmetry reduction, retaining every safe one-item and two-item move.
-}
module AoC.Y2016.Day11.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day11.Input (Floor (FloorFour, FloorOne, FloorThree, FloorTwo), Input (Input), parseInput)
import Data.Set qualified as Set

data Arrangement = Arrangement Floor [(Floor, Floor)]
    deriving stock (Eq, Ord)

solver :: Solver
solver text = do
    Input pairs <- parseInput text
    steps <- shortest pairs
    pure (Answers (show steps) (Just "0"))

shortest :: [(Floor, Floor)] -> Either Text Int
shortest pairs
    | safe pairs = search 0 (one initial) (one initial)
    | otherwise = Left "Initial arrangement exposes an unpowered microchip"
  where
    initial = Arrangement FloorOne (sort pairs)

search :: Int -> Set Arrangement -> Set Arrangement -> Either Text Int
search distance seen frontier
    | any complete frontier = Right distance
    | Set.null frontier = Left "No safe elevator route reaches the fourth floor"
    | otherwise = search (distance + 1) (Set.union seen next) next
  where
    next = frontier & Set.toList & concatMap neighbours & Set.fromList & (`Set.difference` seen)

complete :: Arrangement -> Bool
complete (Arrangement _ pairs) = all (== (FloorFour, FloorFour)) pairs

safe :: [(Floor, Floor)] -> Bool
safe pairs = all (\(generator, microchip) -> generator == microchip || not (Set.member microchip generators)) pairs
  where
    generators = pairs <&> fst & Set.fromList

neighbours :: Arrangement -> [Arrangement]
neighbours (Arrangement elevator pairs) =
    [ Arrangement destination (sort moved)
    | destination <- adjacent elevator
    , cargo <- cargoes items
    , let moved = moveCargo destination cargo pairs
    , safe moved
    ]
  where
    items = [(index, generator) | (index, (g, m)) <- zip [0 ..] pairs, (generator, floorNumber) <- [(True, g), (False, m)], floorNumber == elevator]

cargoes :: [a] -> [[a]]
cargoes items = map one items <> [[a, b] | (index, a) <- zip [0 :: Int ..] items, b <- drop (index + 1) items]

moveCargo :: Floor -> [(Int, Bool)] -> [(Floor, Floor)] -> [(Floor, Floor)]
moveCargo destination cargo pairs =
    [ (relocate index True generator, relocate index False microchip)
    | (index, (generator, microchip)) <- zip [0 ..] pairs
    ]
  where
    relocate index component original
        | (index, component) `elem` cargo = destination
        | otherwise = original

adjacent :: Floor -> [Floor]
adjacent = \case
    FloorOne -> [FloorTwo]
    FloorTwo -> [FloorOne, FloorThree]
    FloorThree -> [FloorTwo, FloorFour]
    FloorFour -> [FloorThree]
