{- |
approach: Count marker payloads directly for v1 and recursively for v2.
cost: O(n * (d + 1)) time and memory for nesting depth d; normalize, parse, then count,
      reparsing each nested payload once without expanding repetitions.
trade: Arbitrary-precision lengths avoid storing decompressed output;
       recursively malformed v1 payloads keep a v2 placeholder of 0.
-}
module AoC.Y2016.Day09.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day09.Input (Chunk (Literal, Repeat), Input (Input), parseInput)
import Data.Text qualified as Text

{- | Return decompressed lengths with literal marker payloads, then recursively
interpreted payloads; malformed nested markers use 0 for part 2. Grammar errors are
returned as text.
-}
solver :: Solver
solver text = do
    input@(Input chunks) <- parseInput text
    let part1 = sum (map chunkLength chunks)
    let part2 = recursiveLength input & fromRight 0
    answers (show part1) (Just (show part2))

chunkLength :: Chunk -> Natural
chunkLength = \case
    Literal text -> textLength text
    Repeat copies text -> copies * textLength text

recursiveLength :: Input -> Either Text Natural
recursiveLength (Input chunks) = sum <$> traverse recursiveChunk chunks

recursiveChunk :: Chunk -> Either Text Natural
recursiveChunk = \case
    Literal text -> Right (textLength text)
    Repeat 0 _ -> Right 0
    Repeat copies text -> do
        input <- parseInput text
        size <- recursiveLength input
        pure (copies * size)

textLength :: Text -> Natural
textLength = Text.foldl' (\size _ -> size + 1) 0
