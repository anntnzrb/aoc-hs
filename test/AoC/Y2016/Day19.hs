module AoC.Y2016.Day19 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day19.Primary (solver)
import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testProperty "2016/19 literal circles" circles

circles :: Property
circles =
    ( do
        count <- forAll (Gen.int (Range.constant 1 300))
        fmap render (solver (show count)) === Right (unlines [show (winner (const 1) [1 .. count]), show (winner (`div` 2) [1 .. count])])
    )
        & property
        & withTests 300

winner :: (Int -> Int) -> [Int] -> Int
winner victim = \case
    [] -> 0
    [elf] -> elf
    elves -> winner victim (rotate (take index elves <> drop (index + 1) elves))
      where
        index = victim (foldl' (\count _ -> count + 1) 0 elves)
        rotate :: [Int] -> [Int]
        rotate [] = []
        rotate (elf : rest) = rest <> [elf]
