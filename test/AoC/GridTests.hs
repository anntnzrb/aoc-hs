module AoC.GridTests (tests) where

import AoC.Grid (adjacent, orthogonal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
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
