{- |
approach: Encode traps as bits; unequal neighbours are the XOR of left and right shifts.
cost: O(width² / word size + rows * ceil(width / word size)) word operations, including repeated-shift seed encoding; O(width) live bits; one parsing pass, two seed passes per answer.
trade: Integer shifts and population counts evolve arbitrary-width rows without per-tile allocation; a mask keeps both walls safe.
-}
module AoC.Y2016.Day18.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day18.Input (Input (Input), Tile (Safe, Trap), parseInput)
import Data.Bits (bit, popCount, shiftL, shiftR, (.&.), (.|.))

{- | Count safe tiles across 40 and 400000 rows, including the supplied first row; tiles
outside the row are safe. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input row <- parseInput text
    answers (show (safeTiles 40 (toList row))) (Just (show (safeTiles 400000 (toList row))))

safeTiles :: Int -> [Tile] -> Int
safeTiles rows row =
    iterate nextRow encoded
        & take rows
        & foldl' countSafe 0
  where
    width = foldl' (\total _ -> total + 1) 0 row
    mask = bit width - 1
    encoded = foldl' (\bits tile -> shiftL bits 1 .|. encode tile) 0 row

    countSafe :: Int -> Integer -> Int
    countSafe total traps = total + width - popCount traps

    nextRow :: Integer -> Integer
    nextRow traps = (shiftL traps 1 `xor` shiftR traps 1) .&. mask

encode :: Tile -> Integer
encode Safe = 0
encode Trap = 1
