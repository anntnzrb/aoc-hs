{- |
approach: Fold operations forward, then undo them in reverse order.
The position-based rotation is inverted by checking every password rotation.
cost: O(m * n^2) time, O(m + n) memory, three input passes for m operations and n letters.
trade: Total sequence edits and a bounded inverse search avoid lookup tables; ambiguous inverses at other lengths fail explicitly.
-}
module AoC.Y2016.Day21.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day21.Input (Direction (Leftward, Rightward), Letter, Operation (Move, Reverse, Rotate, RotateLetter, SwapLetter, SwapPosition), Span, bounds, letterValue, parseInput)
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
    SwapPosition x y -> swapPositions x y password
    SwapLetter a b -> pure (password <&> exchange (letterValue a) (letterValue b))
    Rotate direction steps -> pure (rotate (signedSteps direction steps) password)
    RotateLetter value -> basedRotation value password
    Reverse interval -> reverseSpan interval password
    Move x y -> movePosition x y password

swapPositions :: Natural -> Natural -> Seq Char -> Either Text (Seq Char)
swapPositions x y password = do
    start <- position x password
    end <- position y password
    a <- at start password
    b <- at end password
    password
        & Seq.update start b
        & Seq.update end a
        & pure

movePosition :: Natural -> Natural -> Seq Char -> Either Text (Seq Char)
movePosition x y password = do
    start <- position x password
    end <- position y password
    value <- at start password
    password
        & Seq.deleteAt start
        & Seq.insertAt end value
        & pure

position :: Natural -> Seq Char -> Either Text Int
position index password
    | index < fromIntegral (Seq.length password) = Right (fromIntegral index)
    | otherwise = Left "position outside password"

at :: Int -> Seq Char -> Either Text Char
at index = Seq.lookup index >>> maybeToRight "position outside password"

signedSteps :: Direction -> Natural -> Integer
signedSteps direction steps = case direction of
    Leftward -> negate (toInteger steps)
    Rightward -> toInteger steps

rotate :: Integer -> Seq Char -> Seq Char
rotate steps password
    | Seq.null password = password
    | otherwise = after <> before
  where
    (before, after) = Seq.splitAt (fromInteger (negate steps `mod` toInteger (Seq.length password))) password

basedRotation :: Letter -> Seq Char -> Either Text (Seq Char)
basedRotation value password = do
    index <- Seq.elemIndexL (letterValue value) password & maybeToRight "letter absent from password"
    pure (rotate (rotationSteps index) password)
  where
    rotationSteps :: Int -> Integer
    rotationSteps index
        | index >= 4 = toInteger index + 2
        | otherwise = toInteger index + 1

exchange :: Char -> Char -> Char -> Char
exchange a b value
    | value == a = b
    | value == b = a
    | otherwise = value

reverseSpan :: Span -> Seq Char -> Either Text (Seq Char)
reverseSpan interval password = do
    start <- position x password
    end <- position y password
    let (before, rest) = Seq.splitAt start password
        (middle, after) = Seq.splitAt (end - start + 1) rest
    pure (before <> Seq.reverse middle <> after)
  where
    (x, y) = bounds interval

undo :: Operation -> Seq Char -> Either Text (Seq Char)
undo operation password = case operation of
    Rotate direction steps -> pure (rotate (negate (signedSteps direction steps)) password)
    Move x y -> apply (Move y x) password
    RotateLetter value -> inverseRotation value password
    SwapPosition{} -> apply operation password
    SwapLetter{} -> apply operation password
    Reverse{} -> apply operation password

-- Position-based rotation is bijective at length eight; other lengths may be ambiguous.
inverseRotation :: Letter -> Seq Char -> Either Text (Seq Char)
inverseRotation value password =
    [candidate | steps <- [0 .. Seq.length password - 1], let candidate = rotate (toInteger steps) password, basedRotation value candidate == Right password]
        & ordNub
        & \case
            [candidate] -> Right candidate
            [] -> Left "position rotation has no inverse"
            _ : _ : _ -> Left "position rotation has ambiguous inverses"
