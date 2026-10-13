{- |
approach: Transpose the recordings and choose each column's most and least frequent letters.
cost: O(r*w*log a) time, O(r*w) memory; four passes: parse, width check, transpose, count.
trade: One frequency map serves both parts; ties choose the larger letter for part 1, smaller for part 2.
-}
module AoC.Y2016.Day06.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day06.Input (Input (Input), parseInput)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    Input rows <- parseInput text
    let (most, least) = rows & NE.transpose & fmap decode & toList & unzip
    pure (Answers (toText most) (Just (toText least)))

decode :: NonEmpty Char -> (Char, Char)
decode letters =
    letters
        & foldl' (\counts letter -> Map.insertWith (+) letter (1 :: Int) counts) Map.empty
        & Map.foldlWithKey'
            (\(most, least) letter count -> (max most (count, letter), min least (count, letter)))
            ((0, head letters), (maxBound, head letters))
        & bimap snd snd
