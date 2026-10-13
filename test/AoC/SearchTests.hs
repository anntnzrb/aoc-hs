module AoC.SearchTests (tests) where

import AoC.Search (breadthFirstLayers, distances, shortestDistance)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "breadth-first search"
        [ testCase "deduplicate edges, cycles and convergent routes" $
            breadthFirstLayers successors 0 @?= [(0, one 0), (1, Set.fromList [1, 2]), (2, one 3), (3, one 4)]
        , testCase "shortest path counts edges" (shortestDistance successors (== 3) 0 @?= Just 2)
        , testCase "origin already matches" (shortestDistance successors (== 0) 0 @?= Just 0)
        , testCase "unreachable target exhausts search" (shortestDistance successors (== 5) 0 @?= Nothing)
        , testCase "all reached distances" (distances successors 0 @?= Map.fromList [(0, 0), (1, 1), (2, 1), (3, 2), (4, 3)])
        , testCase "isolated origin" (breadthFirstLayers successors 5 @?= [(0, one 5)])
        , testCase "edges are directed" (shortestDistance successors (== 1) 4 @?= Nothing)
        , testCase "finite prefix of infinite graph" (shortestDistance (\n -> [n + 1]) (== (5 :: Integer)) 0 @?= Just 5)
        ]

successors :: Int -> [Int]
successors = \case
    0 -> [0, 1, 1, 2]
    1 -> [0, 3]
    2 -> [3]
    3 -> [2, 4]
    _ -> []
