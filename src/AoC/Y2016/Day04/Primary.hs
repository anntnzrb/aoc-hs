{- |
approach: Rank letter counts to identify real rooms, then rotate letters to find storage.
cost: O(n) time and O(n) memory; parsing, classification/sum, then a real-room search.
trade: A strict map has at most 26 entries per room; absent storage keeps a zero placeholder.
-}
module AoC.Y2016.Day04.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day04.Input (Room (checksum, sector, words), parseInput)
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    rooms <- parseInput text
    let genuine = filter real rooms
    pure
        ( Answers
            (show (sum [room.sector | room <- genuine]))
            (Just (show (maybe 0 (\room -> room.sector) (find storage genuine))))
        )

storage :: Room -> Bool
storage room =
    room.words
        & toList
        & map decrypt
        & unwords
        & (== "northpole object storage")
  where
    decrypt :: Text -> Text
    decrypt =
        toString
            >>> map (rotate room.sector)
            >>> toText

rotate :: Natural -> Char -> Char
rotate sector letter = chr (ord 'a' + (ord letter - ord 'a' + fromIntegral (sector `mod` 26)) `mod` 26)

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
