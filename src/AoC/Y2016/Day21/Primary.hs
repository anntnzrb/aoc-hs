{- |
approach: Fold operations forward, then undo them in reverse order.
The position-based rotation is inverted by checking all eight rotations.
cost: O(m * n^2) time, O(m + n) memory, three input passes; n = 8 letters.
trade: Total sequence edits and a bounded inverse search avoid lookup tables.
-}
module AoC.Y2016.Day21.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day21.Input (Operation (Move, Reverse, Rotate, RotateLetter, SwapLetter, SwapPosition), parseInput)
import Data.Foldable qualified as Foldable
import Data.Sequence qualified as Seq

solver :: Solver
solver text = do
    operations <- parseInput text
    scrambled <- foldlM (flip apply) (Seq.fromList "abcdefgh") operations
    original <- Foldable.foldrM undo (Seq.fromList "fbgdceah") operations
    pure (Answers (toText (toList scrambled)) (Just (toText (toList original))))

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

undo :: Operation -> Seq Char -> Either Text (Seq Char)
undo operation password = case operation of
    Rotate steps -> pure (rotate (negate steps) password)
    Move x y -> apply (Move y x) password
    RotateLetter value ->
        [candidate | steps <- [0 .. 7], let candidate = rotate steps password, basedRotation value candidate == Right password]
            & viaNonEmpty head
            & maybeToRight "position rotation has no inverse"
    SwapPosition{} -> apply operation password
    SwapLetter{} -> apply operation password
    Reverse{} -> apply operation password
