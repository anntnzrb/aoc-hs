module AoC.Y2016.Day21 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day21.Input (parseInput)
import AoC.Y2016.Day21.Primary (solver)
import Hedgehog (Gen, Property, evalEither, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

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
        , testProperty "valid operations preserve both password permutations" passwordPermutations
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
    result <- evalEither (solver (unlines operations))
    map (toString >>> sort) (lines (render result)) === map sort ["abcdefgh", "fbgdceah"]

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
