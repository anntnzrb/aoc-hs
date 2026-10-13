module AoC.Y2016.Tests (tests) where

import AoC.Y2016.Day11 qualified as Day11
import AoC.Y2016.Day16 qualified as Day16
import AoC.Y2016.Day18 qualified as Day18
import AoC.Y2016.Day19 qualified as Day19
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "2016 differential properties" [Day11.tests, Day16.tests, Day18.tests, Day19.tests]
