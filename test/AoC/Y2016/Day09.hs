module AoC.Y2016.Day09 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day09.Input qualified as Compressed
import AoC.Y2016.Day09.Primary qualified as Primary
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

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
        , testCase "zero copies discard malformed nested payload" $
            (render <$> Primary.solver "A(1x0)(B") @?= Right "2\n2\n"
            -- ops:property-entries
        ]
