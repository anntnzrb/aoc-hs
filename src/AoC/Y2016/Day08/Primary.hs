{- |
approach: Fold instructions over lit coordinates; rotations wrap their selected line; read 5x6 glyphs.
cost: O(n * s log s) time, O(n + s) memory; parse and fold passes, s <= 300.
trade: A set makes overlaps idempotent; exact font matching leaves non-text screens at 0.
-}
module AoC.Y2016.Day08.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day08.Input (Instruction (Rect, RotateColumn, RotateRow), parseInput)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Screen = Set (Int, Int)

solver :: Solver
solver text = do
    instructions <- parseInput text
    let screen = foldl' apply Set.empty instructions
    pure (Answers (show (Set.size screen)) (Just (decode screen)))

apply :: Screen -> Instruction -> Screen
apply screen = \case
    Rect width height -> screen `Set.union` Set.fromList [(x, y) | y <- [0 .. height - 1], x <- [0 .. width - 1]]
    RotateRow row amount -> screen & Set.map (\(x, y) -> (if y == row then (x + amount) `mod` 50 else x, y))
    RotateColumn column amount -> screen & Set.map (\(x, y) -> (x, if x == column then (y + amount) `mod` 6 else y))

-- Non-letter displays, including the part-1 example, have no code and print 0.
decode :: Screen -> Text
decode screen = [0, 5 .. 45] & traverse (glyph >>> (`Map.lookup` alphabet)) & maybe "0" toText
  where
    glyph start = [if Set.member (x, y) screen then '#' else '.' | y <- [0 .. 5], x <- [start .. start + 4]] & toText

-- Visual transcription of the observed 5x6 capital-letter font, not a code token.
alphabet :: Map Text Char
alphabet =
    Map.fromList
        [ ("####." <> "...#." <> "..#.." <> ".#..." <> "#...." <> "####.", 'Z')
        , ("..##." <> "...#." <> "...#." <> "...#." <> "#..#." <> ".##..", 'J')
        , ("#..#." <> "#..#." <> "####." <> "#..#." <> "#..#." <> "#..#.", 'H')
        , ("###.." <> "#..#." <> "#..#." <> "###.." <> "#.#.." <> "#..#.", 'R')
        , ("#..#." <> "#.#.." <> "##..." <> "#.#.." <> "#.#.." <> "#..#.", 'K')
        , (".##.." <> "#..#." <> "#...." <> "#...." <> "#..#." <> ".##..", 'C')
        , ("###.." <> "#..#." <> "#..#." <> "###.." <> "#...." <> "#....", 'P')
        , ("#...." <> "#...." <> "#...." <> "#...." <> "#...." <> "####.", 'L')
        , ("#...#" <> "#...#" <> ".#.#." <> "..#.." <> "..#.." <> "..#..", 'Y')
        ]
