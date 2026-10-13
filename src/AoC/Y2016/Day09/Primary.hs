{- |
approach: Sum literal lengths and marker payload lengths times their repetitions.
cost: O(n) time and memory; whitespace filtering, parsing and chunk traversal.
trade: Keep payloads compressed, counting their size without building output.
-}
module AoC.Y2016.Day09.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day09.Input (Chunk (Literal, Repeat), Input (Input), parseInput)

solver :: Solver
solver text = do
    Input chunks <- parseInput text
    pure (Answers (show (sum (map chunkLength chunks))) (Just "0"))

chunkLength :: Chunk -> Natural
chunkLength = \case
    Literal text -> textLength text
    Repeat copies text -> copies * textLength text

textLength :: Text -> Natural
textLength text = foldl' (\n _ -> n + 1) 0 (toString text)
