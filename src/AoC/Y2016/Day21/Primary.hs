{- |
approach: Fold the parsed operations over the eight-letter password.
cost: O(m * n) time, O(m + n) memory, two input passes; n = 8 letters.
trade: A sequence keeps position edits total and the operation rules explicit.
-}
module AoC.Y2016.Day21.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day21.Input (Operation (Move, Reverse, Rotate, RotateLetter, SwapLetter, SwapPosition), parseInput)
import Data.Sequence qualified as Seq

solver :: Solver
solver text = do
    operations <- parseInput text
    scrambled <- foldlM (flip apply) (Seq.fromList "abcdefgh") operations
    pure (Answers (toText (toList scrambled)) (Just "0"))

apply :: Operation -> Seq Char -> Either Text (Seq Char)
apply operation password = case operation of
    SwapPosition x y -> do
        a <- at x password
        b <- at y password
        pure (password & Seq.update x b & Seq.update y a)
    SwapLetter a b -> pure (password <&> exchange a b)
    Rotate steps -> pure (rotate steps password)
    RotateLetter value -> basedRotation value password
    Reverse x y -> pure (reverseSpan x y password)
    Move x y -> do
        value <- at x password
        pure (password & Seq.deleteAt x & Seq.insertAt y value)

at :: Int -> Seq Char -> Either Text Char
at index = Seq.lookup index >>> maybeToRight "position outside password"

-- The password always has eight letters; parsed operations preserve its length.
rotate :: Int -> Seq Char -> Seq Char
rotate steps password = after <> before
  where
    (before, after) = Seq.splitAt (negate steps `mod` 8) password

basedRotation :: Char -> Seq Char -> Either Text (Seq Char)
basedRotation value password = do
    index <- Seq.elemIndexL value password & maybeToRight "letter absent from password"
    pure (rotate (1 + index + if index >= 4 then 1 else 0) password)

exchange :: Char -> Char -> Char -> Char
exchange a b value
    | value == a = b
    | value == b = a
    | otherwise = value

reverseSpan :: Int -> Int -> Seq Char -> Seq Char
reverseSpan x y password = before <> Seq.reverse middle <> after
  where
    (before, rest) = Seq.splitAt x password
    (middle, after) = Seq.splitAt (y - x + 1) rest
