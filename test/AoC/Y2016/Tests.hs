module AoC.Y2016.Tests (tests) where

import AoC.Y2016.Day05 qualified as Day05
import AoC.Y2016.Day08 qualified as Day08
import AoC.Y2016.Day09 qualified as Day09
import AoC.Y2016.Day10 qualified as Day10
import AoC.Y2016.Day11 qualified as Day11
import AoC.Y2016.Day16 qualified as Day16
import AoC.Y2016.Day18 qualified as Day18
import AoC.Y2016.Day19 qualified as Day19
import AoC.Y2016.Day21 qualified as Day21
import AoC.Y2016.Day22 qualified as Day22
import AoC.Y2016.Day24 qualified as Day24
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
    testGroup
        "2016 differential properties"
        [ Day05.tests
        , Day08.tests
        , Day09.tests
        , Day10.tests
        , Day11.tests
        , Day16.tests
        , Day18.tests
        , Day19.tests
        , Day21.tests
        , Day22.tests
        , Day24.tests
        ]
