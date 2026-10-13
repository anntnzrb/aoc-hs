module AoC.Y2016.Day21 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day21.Input (Direction (Leftward, Rightward), Operation (Move, Reverse, Rotate, RotateLetter, SwapLetter, SwapPosition), bounds, letterValue, parseInput)
import AoC.Y2016.Day21.Primary (solver)
import Data.List (elemIndex)
import Data.Text qualified as T
import Hedgehog (Gen, Property, evalEither, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

-- ops:property-imports

tests :: TestTree
tests =
    testGroup
        "2016/21"
        [ testCase "out-of-range positions fail" $
            for_ invalidPositions (\input -> isLeft (solver input) @?= True)
        , testCase
            "huge rotation counts parse and wrap"
            ( for_ [("left", "bcdefgha\nhfbgdcea\n"), ("right", "habcdefg\nbgdceahf\n")] $ \(direction, expected) -> do
                let huge = "rotate " <> direction <> " 184467440737095516160000000000000000001 steps"
                isRight (parseInput huge) @?= True
                fmap render (solver huge) @?= Right expected
            )
        , testCase "rotation by an absent letter fails" $
            isLeft (solver "rotate based on position of letter z") @?= True
        , testCase "reversed spans are rejected" $
            isLeft (parseInput "reverse positions 6 through 2") @?= True
        , testProperty "literal scrambling and inverse agree" passwordPermutations
        -- ops:property-entries
        ]

invalidPositions :: [Text]
invalidPositions =
    [ "swap position 8 with position 0"
    , "swap position 0 with position 8"
    , "move position 8 to position 0"
    , "move position 0 to position 8"
    , "reverse positions 0 through 8"
    , "reverse positions 8 through 8"
    , "swap position 18446744073709551616 with position 0"
    ]

passwordPermutations :: Property
passwordPermutations = property $ do
    operations <- forAll (Gen.list (Range.linear 1 80) operation)
    parsed <- evalEither (parseInput (unlines operations))
    result <- evalEither (solver (unlines operations))
    case lines (render result) of
        [scrambled, recovered] -> do
            scrambled === toText (scramble parsed "abcdefgh")
            toText (scramble parsed (toString recovered)) === "fbgdceah"
        _outputs -> failure

operation :: Gen Text
operation = do
    x <- Gen.int (Range.constant 0 7)
    y <- Gen.int (Range.constant 0 7)
    a <- Gen.element ['a' .. 'h']
    b <- Gen.element ['a' .. 'h']
    steps <- Gen.integral (Range.constant 0 1000000000000000000000000) :: Gen Integer
    Gen.element
        [ "swap position " <> show x <> " with position " <> show y
        , "swap letter " <> one a <> " with letter " <> one b
        , "rotate left " <> show steps <> " steps"
        , "rotate right " <> show steps <> " steps"
        , "rotate based on position of letter " <> one a
        , "reverse positions " <> show (min x y) <> " through " <> show (max x y)
        , "move position " <> show x <> " to position " <> show y
        ]

-- A literal list edit for each statement operation; no inverse implementation.
scramble :: [Operation] -> [Char] -> [Char]
scramble operations password = foldl' edit password operations

edit :: [Char] -> Operation -> [Char]
edit password = \case
    SwapPosition x y -> [at (if i == fromIntegral x then fromIntegral y else if i == fromIntegral y then fromIntegral x else i) | i <- [0 .. 7]]
    SwapLetter a b -> map (\letter -> if letter == letterValue a then letterValue b else if letter == letterValue b then letterValue a else letter) password
    Rotate direction count -> rotateList (case direction of Leftward -> count; Rightward -> (8 - count `mod` 8) `mod` 8) password
    RotateLetter letter -> rotateLetter (letterValue letter) password
    Reverse spanValue -> reverseSpan (bounds spanValue)
    Move x y -> moveLetter (fromIntegral x) (fromIntegral y)
  where
    at index = fromMaybe '?' (viaNonEmpty head (drop index password))
    reverseSpan :: (Natural, Natural) -> [Char]
    reverseSpan (start, end) = take lo password <> (take size (drop lo password) & toText & T.reverse & toString) <> drop (lo + size) password
      where
        lo = fromIntegral start
        size = fromIntegral (end - start + 1)
    moveLetter source target = take target remaining <> [at source] <> drop target remaining
      where
        remaining = take source password <> drop (source + 1) password

rotateLetter :: Char -> [Char] -> [Char]
rotateLetter letter password = rotateList (fromIntegral ((8 - amount `mod` 8) `mod` 8)) password
  where
    index = fromMaybe 0 (elemIndex letter password)
    amount = 1 + index + if index >= 4 then 1 else 0

rotateList :: Natural -> [Char] -> [Char]
rotateList count password = drop steps password <> take steps password
  where
    steps = fromIntegral (count `mod` 8)
