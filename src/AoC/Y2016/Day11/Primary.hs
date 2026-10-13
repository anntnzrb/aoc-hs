{- |
approach: Breadth-first elevator search; sort generator/chip floor pairs to identify equivalent element renamings.
cost: O(V n^3 log V) time and O(V + n) memory with packed keys, O(V n) memory for oversized inventories; one parsing pass.
trade: Canonical pairs fit compact IntSet keys when their encoding cannot overflow; a four-bit floor mask replaces generator sets. Oversized inventories retain the structural search; unsafe part-2 starts print 0.
-}
module AoC.Y2016.Day11.Primary (solver) where

import AoC.Search (shortestDistance, shortestIntDistance)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day11.Input (Floor (FloorFour, FloorOne, FloorThree, FloorTwo), Input (Input), parseInput)
import Data.Bits (bit, testBit, (.|.))

data Arrangement = Arrangement Floor [(Floor, Floor)]
    deriving stock (Eq, Ord)

solver :: Solver
solver text = do
    Input pairs <- parseInput text
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
    count = length pairs
    space = 16 ^ count :: Integer
    base = fromInteger space
    successors = unpackArrangement count base >>> neighbours >>> map packArrangement

packArrangement :: Arrangement -> Int
packArrangement (Arrangement elevator pairs) = foldl' (\encoded (generator, microchip) -> 16 * encoded + 4 * floorCode generator + floorCode microchip) (floorCode elevator) pairs

unpackArrangement :: Int -> Int -> Int -> Arrangement
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
