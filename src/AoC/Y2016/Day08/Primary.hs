{- |
approach: Fold instructions over lit coordinates; rotations wrap their selected line; read 5x6 glyphs.
cost: O(n * s log s) time, O(n + s) memory; parse and fold passes, s <= 300.
trade: A set makes overlaps idempotent; exact font matching leaves non-text screens at 0.
-}
module AoC.Y2016.Day08.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day08.Input (Column (column), Height (height), Instruction (Rect, RotateColumn, RotateRow), Row (row), Width (width), parseInput)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

type Screen = Set (Int, Int)

solver :: Solver
solver text = do
    instructions <- parseInput text
    let screen = foldl' apply Set.empty instructions
    pure (Answers (show (Set.size screen)) (Just (fromMaybe "0" (decode screen))))

apply :: Screen -> Instruction -> Screen
apply screen = \case
    Rect extent tall -> screen `Set.union` Set.fromList [(x, y) | y <- [0 .. tall.height - 1], x <- [0 .. extent.width - 1]]
    RotateRow line amount -> screen & Set.map (rotateRow line.row (fromIntegral (amount `mod` 50)))
    RotateColumn line amount -> screen & Set.map (rotateColumn line.column (fromIntegral (amount `mod` 6)))

rotateRow :: Int -> Int -> (Int, Int) -> (Int, Int)
rotateRow row amount (x, y)
    | y == row = ((x + amount) `mod` 50, y)
    | otherwise = (x, y)

rotateColumn :: Int -> Int -> (Int, Int) -> (Int, Int)
rotateColumn column amount (x, y)
    | x == column = (x, (y + amount) `mod` 6)
    | otherwise = (x, y)

-- Non-letter displays, including the part-1 example, have no code and print 0.
decode :: Screen -> Maybe Text
decode screen =
    [0, 5 .. 45]
        & traverse (glyph >>> (`Map.lookup` alphabet))
        <&> toText
  where
    glyph start = [if Set.member (x, y) screen then '#' else '.' | y <- [0 .. 5], x <- [start .. start + 4]] & toText

-- Visual transcription of the 5x6 capital-letter font. Only the originally
-- observed Z, J, H, R, K, C, P, L and Y glyphs are verified against real output.
alphabet :: Map Text Char
alphabet =
    Map.fromList
        [ (".##.." <> "#..#." <> "#..#." <> "####." <> "#..#." <> "#..#.", 'A')
        , ("###.." <> "#..#." <> "###.." <> "#..#." <> "#..#." <> "###..", 'B')
        , ("####." <> "#...." <> "###.." <> "#...." <> "#...." <> "####.", 'E')
        , ("####." <> "#...." <> "###.." <> "#...." <> "#...." <> "#....", 'F')
        , (".##.." <> "#..#." <> "#...." <> "#.##." <> "#..#." <> ".###.", 'G')
        , (".##.." <> "#..#." <> "#..#." <> "#..#." <> "#..#." <> ".##..", 'O')
        , (".###." <> "#...." <> "#...." <> ".##.." <> "...#." <> "###..", 'S')
        , ("#..#." <> "#..#." <> "#..#." <> "#..#." <> "#..#." <> ".##..", 'U')
        , ("####." <> "...#." <> "..#.." <> ".#..." <> "#...." <> "####.", 'Z')
        , ("..##." <> "...#." <> "...#." <> "...#." <> "#..#." <> ".##..", 'J')
        , ("#..#." <> "#..#." <> "####." <> "#..#." <> "#..#." <> "#..#.", 'H')
        , ("###.." <> "#..#." <> "#..#." <> "###.." <> "#.#.." <> "#..#.", 'R')
        , ("#..#." <> "#.#.." <> "##..." <> "#.#.." <> "#.#.." <> "#..#.", 'K')
        , (".##.." <> "#..#." <> "#...." <> "#...." <> "#..#." <> ".##..", 'C')
        , ("###.." <> "#..#." <> "#..#." <> "###.." <> "#...." <> "#....", 'P')
        , ("#...." <> "#...." <> "#...." <> "#...." <> "#...." <> "####.", 'L')
        , ("#...#" <> "#...#" <> ".#.#." <> "..#.." <> "..#.." <> "..#..", 'Y')
        ]
