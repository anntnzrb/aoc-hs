{- |
approach: Transpose the recordings and choose each column's most frequent letter.
cost: O(r*w*log a) time, O(r*w) memory; four passes: parse, width check, transpose, count.
trade: Strict frequency maps keep decoding direct; tied counts choose the larger letter.
-}
module AoC.Y2016.Day06.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day06.Input (Input (Input), parseInput)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    Input rows <- parseInput text
    pure (Answers (rows & NE.transpose & fmap mostCommon & toList & toText) (Just "0"))

mostCommon :: NonEmpty Char -> Char
mostCommon letters =
    letters
        & foldl' (\counts letter -> Map.insertWith (+) letter (1 :: Int) counts) Map.empty
        & Map.foldlWithKey' (\best letter count -> max best (count, letter)) (0, head letters)
        & snd
