{- |
approach: Evolve rows with safe walls; the four trap rules mean unequal neighbours.
cost: O(rows * width) time, O(width) live memory; one input parse pass.
trade: Streaming lists express the local rule directly without storing the room.
-}
module AoC.Y2016.Day18.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day18.Input (Input (Input), Tile (Safe, Trap), parseInput)

solver :: Solver
solver text = do
    Input row <- parseInput text
    pure (Answers (show (safeTiles 40 (toList row))) (Just "0"))

safeTiles :: Int -> [Tile] -> Int
safeTiles rows = iterate nextRow >>> take rows >>> foldl' countSafe 0
  where
    countSafe :: Int -> [Tile] -> Int
    countSafe total row = foldl' countTile total row

    countTile :: Int -> Tile -> Int
    countTile total Safe = total + 1
    countTile total Trap = total

nextRow :: [Tile] -> [Tile]
nextRow row = zipWith tile (Safe : row) (drop 1 row <> [Safe])
  where
    tile :: Tile -> Tile -> Tile
    tile left right
        | left == right = Safe
        | otherwise = Trap
