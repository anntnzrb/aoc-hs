-- | Register shared-library semantics together, independently of day fixtures.
module AoC.SharedTests (tests) where

import AoC.AssembunnyTests qualified as Assembunny
import AoC.GridTests qualified as Grid
import AoC.MD5Tests qualified as MD5
import AoC.ParseTests qualified as Parse
import AoC.SearchTests qualified as Search
import AoC.SolutionTests qualified as Solution
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "shared" [Assembunny.tests, Grid.tests, MD5.tests, Parse.tests, Search.tests, Solution.tests]
