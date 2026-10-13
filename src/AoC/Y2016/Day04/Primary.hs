{- |
approach: Count name letters, rank by descending frequency then alphabet, sum real sectors.
cost: O(n) time and O(n) parsed input memory; two input passes, parsing and classification.
trade: A strict map expresses counts directly; at most 26 entries per room. Part 2 unfinished.
-}
module AoC.Y2016.Day04.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day04.Input (Room (checksum, sector, words), parseInput)
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    rooms <- parseInput text
    pure (Answers (show (sum [room.sector | room <- rooms, real room])) (Just "0"))

real :: Room -> Bool
real room =
    room.words
        & toList
        & concatMap toString
        & map (,1 :: Int)
        & Map.fromListWith (+)
        & Map.toList
        & sortOn (\(letter, frequency) -> (Down frequency, letter))
        & take 5
        & map fst
        & toText
        & (== room.checksum)
