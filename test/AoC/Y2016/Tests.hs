module AoC.Y2016.Tests (tests) where

import AoC.Y2016.Day11 qualified as Day11
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "2016 differential properties" [Day11.tests]
