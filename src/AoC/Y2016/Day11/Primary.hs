{- |
approach: Breadth-first elevator search; sort generator/chip floor pairs to identify equivalent element renamings.
cost: O(V n^3 log V) time and O(V + n) memory with packed keys, O(V n) memory for oversized inventories; one parsing pass.
trade: Canonical pairs fit compact IntSet keys when their encoding cannot overflow; a four-bit floor mask replaces generator sets. Oversized inventories retain the structural search; unsafe part-2 starts print 0.
-}
module AoC.Y2016.Day11.Primary (solver) where

import AoC.Search (shortestDistance, shortestIntDistance)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day11.Input (Component (Generator, Microchip), Floor (FloorFour, FloorOne, FloorThree, FloorTwo), Input (Input), Locations (generatorFloor, microchipFloor), parseInput)
import Data.Bits (bit, testBit, (.|.))
import Data.Map.Strict qualified as Map

data Arrangement = Arrangement Floor [(Floor, Floor)]
    deriving stock (Eq, Ord)

solver :: Solver
solver text = do
    Input elements <- parseInput text
    let pairs = Map.elems elements & map (\locations -> (locations.generatorFloor, locations.microchipFloor))
    steps <- shortest pairs
    pure (Answers (show steps) (Just (either (const "0") show (shortest ((FloorOne, FloorOne) : (FloorOne, FloorOne) : pairs)))))

shortest :: [(Floor, Floor)] -> Either Text Int
shortest pairs
    | safe pairs = packedShortest initial & maybeToRight "No safe elevator route reaches the fourth floor"
    | otherwise = Left "Initial arrangement exposes an unpowered microchip"
  where
    initial = Arrangement FloorOne (sort pairs)

packedShortest :: Arrangement -> Maybe Int
packedShortest initial@(Arrangement _ pairs)
    | 4 * space > toInteger (maxBound :: Int) = shortestDistance neighbours complete initial
    | otherwise = shortestIntDistance successors ((== base - 1) . (`mod` base)) (packArrangement initial)
  where
    count = foldl' (\total _ -> total + 1) 0 pairs
    space = 16 ^ count :: Integer
    base = fromInteger space
    successors = unpackArrangement count base >>> neighbours >>> map packArrangement

packArrangement :: Arrangement -> Int
packArrangement (Arrangement elevator pairs) = foldl' (\encoded (generator, microchip) -> 16 * encoded + 4 * floorCode generator + floorCode microchip) (floorCode elevator) pairs

unpackArrangement :: Int -> Int -> Int -> Arrangement
-- The fixed pair count preserves leading zero pairs. Sorted pairs decode
-- least-significant-first (reversed); successors re-sort before repacking,
-- so the reversed decoding still produces canonical keys.
unpackArrangement count base encoded = Arrangement (codeFloor (encoded `div` base)) [(codeFloor (pair `div` 4), codeFloor (pair `mod` 4)) | digit <- take count (iterate (`div` 16) encoded), let pair = digit `mod` 16]

floorCode :: Floor -> Int
floorCode = \case
    FloorOne -> 0
    FloorTwo -> 1
    FloorThree -> 2
    FloorFour -> 3

codeFloor :: Int -> Floor
codeFloor = \case
    0 -> FloorOne
    1 -> FloorTwo
    2 -> FloorThree
    _ -> FloorFour

complete :: Arrangement -> Bool
complete (Arrangement _ pairs) = all (== (FloorFour, FloorFour)) pairs

safe :: [(Floor, Floor)] -> Bool
safe pairs = all (\(generator, microchip) -> generator == microchip || not (testBit generators (floorCode microchip))) pairs
  where
    generators = foldl' (\mask (generator, _) -> mask .|. bit (floorCode generator)) (0 :: Int) pairs

neighbours :: Arrangement -> [Arrangement]
neighbours (Arrangement elevator pairs) =
    [ Arrangement destination (sort moved)
    | destination <- adjacent elevator
    , cargo <- cargoes items
    , let moved = moveCargo destination cargo pairs
    , safe moved
    ]
  where
    items = [(index, component) | (index, (generator, microchip)) <- zip [0 ..] pairs, (component, floorNumber) <- [(Generator, generator), (Microchip, microchip)], floorNumber == elevator]

cargoes :: [a] -> [[a]]
cargoes items = map one items <> [[a, b] | (index, a) <- zip [0 :: Int ..] items, b <- drop (index + 1) items]

moveCargo :: Floor -> [(Int, Component)] -> [(Floor, Floor)] -> [(Floor, Floor)]
moveCargo destination cargo pairs =
    [ foldl' move pair [component | (item, component) <- cargo, item == index]
    | (index, pair) <- zip [0 ..] pairs
    ]
  where
    move (_, microchip) Generator = (destination, microchip)
    move (generator, _) Microchip = (generator, destination)

adjacent :: Floor -> [Floor]
adjacent = \case
    FloorOne -> [FloorTwo]
    FloorTwo -> [FloorOne, FloorThree]
    FloorThree -> [FloorTwo, FloorFour]
    FloorFour -> [FloorThree]
