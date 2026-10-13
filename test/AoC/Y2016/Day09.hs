module AoC.Y2016.Day09 (tests) where

import AoC.Y2016.Day09.Input qualified as Compressed
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- ops:property-imports

tests :: TestTree
tests =
    testGroup
        "2016/09"
        [ testGroup
            "domain parsing"
            [ testCase "marker width overflow" $
                assertBool
                    "overflowing width accepted"
                    (isLeft (Compressed.parseInput ("(" <> show (toInteger (maxBound @Int) + 1) <> "x1)A")))
            ]
            -- ops:property-entries
        ]
