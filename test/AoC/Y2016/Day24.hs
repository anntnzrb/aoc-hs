module AoC.Y2016.Day24 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day24.Primary (solver)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

type Position = (Int, Int)

tests :: TestTree
tests = testGroup "2016/24 coordinate routes" [testProperty "tiny maps and sparse labels" maps, testCase "disconnected targets" (fmap render (solver "0#9\n") @?= Left "Unreachable numbered location")]

maps :: Property
maps =
    ( do
        tiles <- forAll (Gen.list (Range.singleton 12) (Gen.element ['.', '#']))
        labels <- forAll (Gen.subsequence ['2', '5', '9'])
        positions <- forAll (Gen.shuffle [1 :: Int .. 11])
        let numbered = Map.fromList ((0, '0') : zip positions labels)
            cells = [Map.findWithDefault tile index numbered | (index, tile) <- zip [0 ..] tiles]
            rows = [take 4 (drop offset cells) & toText | offset <- [0, 4, 8]]
            input = unlines rows
        fmap render (solver input) === reference rows
    )
        & property
        & withTests 50

reference :: [Text] -> Either Text Text
reference rows = do
    start <- Map.lookup '0' (Map.fromList labels) & maybeToRight "Missing start"
    let routes = [start : route | route <- permutations [position | (label, position) <- labels, label /= '0']]
        costs returning = mapMaybe (routeCost passages . (\route -> route <> [start | returning])) routes
    outward <- minimumMaybe (costs False) & maybeToRight "Unreachable numbered location"
    homeward <- minimumMaybe (costs True) & maybeToRight "Cannot return to starting location"
    pure (unlines [show outward, show homeward])
  where
    cells = [((x, y), tile) | (y, row) <- zip [0 ..] rows, (x, tile) <- zip [0 ..] (toString row)]
    passages = Set.fromList [position | (position, tile) <- cells, tile /= '#']
    labels = [(tile, position) | (position, tile) <- cells, tile >= '0', tile <= '9']
    minimumMaybe :: [Int] -> Maybe Int
    minimumMaybe = viaNonEmpty (\(value :| rest) -> foldl' min value rest)

routeCost :: Set Position -> [Position] -> Maybe Int
routeCost passages route = traverse (uncurry (distance passages)) (zip route (drop 1 route)) <&> sum

distance :: Set Position -> Position -> Position -> Maybe Int
distance passages start target = layers Set.empty 0 [start]
  where
    layers seen depth frontier
        | target `elem` frontier = Just depth
        | null fresh = Nothing
        | otherwise = layers (Set.union seen (Set.fromList fresh)) (depth + 1) (concatMap neighbours fresh)
      where
        fresh = Set.toList (Set.difference (Set.fromList frontier) seen)
    neighbours (x, y) = filter (`Set.member` passages) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
