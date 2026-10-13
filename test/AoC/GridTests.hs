module AoC.GridTests (tests) where

import AoC.Grid (adjacent, indexedOrthogonal, orthogonal)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "grid" [adjacencyTests, indexingTests]

adjacencyTests :: TestTree
adjacencyTests =
    testGroup
        "grid adjacency"
        [ testCase "unit offsets and stable axis order" (orthogonal (2 :: Int, 3) @?= [(1, 3), (3, 3), (2, 2), (2, 4)])
        , testCase "negative coordinates are retained" (orthogonal (0 :: Integer, 0) @?= [(-1, 0), (1, 0), (0, -1), (0, 1)])
        , testCase "neighbours are adjacent symmetrically" $
            [(adjacent origin position, adjacent position origin) | position <- orthogonal origin]
                @?= replicate
                    4
                    (True, True)
        , testCase "unsigned neighbours are adjacent symmetrically" $
            [(adjacent unsignedOrigin position, adjacent position unsignedOrigin) | position <- orthogonal unsignedOrigin]
                @?= replicate
                    4
                    (True, True)
        , testCase "same, diagonal and distant points are not adjacent" $
            map (adjacent origin) [origin, (1, 1), (2, 0)] @?= [False, False, False]
        ]
  where
    origin :: (Int, Int)
    origin = (0, 0)

    unsignedOrigin :: (Word, Word)
    unsignedOrigin = (2, 3)

indexingTests :: TestTree
indexingTests =
    testGroup
        "indexed orthogonal grid"
        [ testCase "dense indices and neighbours respect their documented order" $
            indexedOrthogonal (Set.fromList [(0 :: Int, 1), (1, 0), (-1, 0), (0, 0), (0, -1), (3, 3)])
                @?= ( Map.fromList [((-1, 0), 0), ((0, -1), 1), ((0, 0), 2), ((0, 1), 3), ((1, 0), 4), ((3, 3), 5)]
                    , IntMap.fromList [(0, [2]), (1, [2]), (2, [0, 4, 1, 3]), (3, [2]), (4, [2]), (5, [])]
                    )
        , testCase "empty grid has empty index and adjacency maps" $
            indexedOrthogonal (Set.empty @(Int, Int)) @?= (Map.empty, IntMap.empty)
        ]
