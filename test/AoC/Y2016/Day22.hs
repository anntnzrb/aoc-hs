module AoC.Y2016.Day22 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day22.Primary (solver)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog (Gen, Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

type Position = (Int, Int)
type Nodes = [(Position, (Int, Int))]
type Storage = (Map Position Int, Position)

tests :: TestTree
tests =
    testGroup
        "2016/22 full storage states"
        [ testProperty "tiny grids with arbitrary capacities" grids
        , testCase "swap preconditions and walls" (traverse_ check examples)
        , testCase "payloads near Int bounds" (traverse_ check largeExamples)
        , testCase "reject overflowing node fields" (traverse_ (\excess -> traverse_ (rejectOverflow excess) [0 .. 5]) [limit, 2 * limit])
        ]
  where
    limit = toInteger (maxBound :: Int) + 1

largeExamples :: [Nodes]
largeExamples =
    [ zip [(0, 0), (1, 0), (0, 1), (1, 1)] [(maxBound, payload), (maxBound, payload), (maxBound, 0), (maxBound, payload)]
    , [((0, 0), (maxBound, maxBound - 1)), ((1, 0), (maxBound, 1))]
    , [((0, 0), (maxBound, maxBound)), ((1, 0), (maxBound, 1))]
    ]
  where
    payload :: Int
    payload = maxBound `div` 2 + 1

rejectOverflow :: Integer -> Int -> IO ()
rejectOverflow excess field = isLeft (solver input) @?= True
  where
    values = [show (value + if index == field then excess else 0) | (index, value) <- zip [0 ..] [0, 0, 1, 0, 1, 0]]
    input =
        zipWith (<>) values ["-y", " ", "T ", "T ", "T ", "%\n"]
            & mconcat
            & ("/dev/grid/node-x" <>)

grids :: Property
grids =
    ( do
        height <- forAll (Gen.int (Range.constant 1 2))
        payloads <- forAll (Gen.list (Range.singleton (2 * height)) node)
        let nodes = zip [(x, y) | y <- [0 .. height - 1], x <- [0, 1]] payloads
        fmap render (solver (encode nodes)) === Right (reference nodes)
    )
        & property
        & withTests 40
  where
    node :: Gen (Int, Int)
    node = do
        capacity <- Gen.int (Range.constant 1 8)
        used <- Gen.int (Range.constant 0 capacity)
        pure (capacity, used)

examples :: [Nodes]
examples = map (zip [(0, 0), (1, 0), (0, 1), (1, 1)]) [[(10, 6), (10, 7), (10, 0), (10, 8)], [(10, 0), (10, 6), (10, 0), (10, 7)], [(10, 6), (10, 7), (100, 99), (10, 0)], [(10, 2), (10, 3), (10, 0), (10, 4)], [(2, 2), (10, 6), (10, 0), (10, 4)], [(10, 6), (10, 7), (100, 80), (10, 0)]]

check :: Nodes -> IO ()
check nodes = fmap render (solver (encode nodes)) @?= Right (reference nodes)

encode :: Nodes -> Text
encode nodes = unlines ("root@grid# df -h" : "Filesystem Size Used Avail Use%" : ["/dev/grid/node-x" <> show x <> "-y" <> show y <> " " <> show capacity <> "T " <> show used <> "T " <> show (capacity - used) <> "T 0%" | ((x, y), (capacity, used)) <- nodes])

reference :: Nodes -> Text
reference nodes = unlines [show viable, show (fromMaybe 0 route)]
  where
    viable = sum [1 :: Int | (a, (_, used)) <- nodes, (b, (capacity, occupied)) <- nodes, a /= b, used > 0, used <= capacity - occupied]
    route = do
        goal <- viaNonEmpty (foldl' max (0, 0)) [position | (position, _) <- nodes, snd position == 0]
        guard (Map.findWithDefault 0 goal loads > 0)
        search capacities (loads, goal)
    capacities = Map.fromList [(position, capacity) | (position, (capacity, _)) <- nodes]
    loads = Map.fromList [(position, used) | (position, (_, used)) <- nodes]

search :: Map Position Int -> Storage -> Maybe Int
search capacities initial = layers Set.empty 0 [initial]
  where
    layers seen depth frontier
        | any ((== (0, 0)) . snd) frontier = Just depth
        | null fresh = Nothing
        | otherwise = layers (Set.union seen (Set.fromList fresh)) (depth + 1) (concatMap (moves capacities) fresh)
      where
        fresh = Set.toList (Set.difference (Set.fromList frontier) seen)

moves :: Map Position Int -> Storage -> [Storage]
moves capacities (loads, goal) = [(Map.insert source 0 (Map.insert target (amount + occupied) loads), if source == goal then target else goal) | (source, amount) <- Map.toList loads, amount > 0, (target, occupied) <- Map.toList loads, adjacent source target, toInteger amount + toInteger occupied <= toInteger (Map.findWithDefault 0 target capacities)]
  where
    adjacent :: Position -> Position -> Bool
    adjacent (x, y) (a, b) = abs (x - a) + abs (y - b) == 1
