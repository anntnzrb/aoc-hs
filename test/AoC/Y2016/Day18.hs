module AoC.Y2016.Day18 (tests) where

import AoC.Solution (Answers (part1), render)
import AoC.Y2016.Day18.Primary (solver)
import Data.List (zipWith3)
import Data.Map.Strict qualified as Map
import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "2016/18 literal rows" [testProperty "arbitrary widths and word boundaries" firstRows, testProperty "both fixed row counts" fullRows]

firstRows :: Property
firstRows =
    ( do
        width <- forAll (Gen.int (Range.constant 1 130))
        for_ (width : [1, 63, 64, 65, 127, 128, 129]) $ \size -> do
            row <- forAll (Gen.list (Range.singleton size) Gen.bool)
            fmap (.part1) (solver (encode row)) === Right (show (literal 40 row))
    )
        & property
        & withTests 12

-- Small widths and widths 2^k-1 have short row cycles; memoize literal rows,
-- never bit encodings, and skip only a cycle already observed in full.
fullRows :: Property
fullRows =
    ( do
        width <- forAll (Gen.element [1, 2, 3, 4, 5, 63, 127])
        row <- forAll (Gen.list (Range.singleton width) Gen.bool)
        fmap render (solver (encode row)) === Right (unlines [show (literal 40 row), show (literal 400000 row)])
    )
        & property
        & withTests 8

encode :: [Bool] -> Text
encode = map (bool '.' '^') >>> toText

literal :: Int -> [Bool] -> Int
literal count = go Map.empty 0 0
  where
    go seen index total row
        | index == count = total
        | Just (previous, subtotal) <- Map.lookup row seen
        , let cycles = (count - index) `div` (index - previous)
        , cycles > 0 =
            go Map.empty (index + cycles * (index - previous)) (total + cycles * (total - subtotal)) row
        | otherwise = go (Map.insert row (index, total) seen) (index + 1) (total + foldl' (\countSafe tile -> countSafe + bool 1 0 tile) 0 row) (next row)

next :: [Bool] -> [Bool]
next row = zipWith3 trap (False : row) row (drop 1 row <> [False])
  where
    trap left center right = (left, center, right) `elem` [(True, True, False), (False, True, True), (True, False, False), (False, False, True)]
