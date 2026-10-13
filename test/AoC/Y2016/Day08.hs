module AoC.Y2016.Day08 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day08.Primary qualified as Screen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests = testGroup "2016/08" [parserCases, screenCases]

parserCases :: TestTree
parserCases =
    testGroup
        "domain parsing"
        [ testCase
            "screen bounds and overflowing dimensions"
            ( for_
                [ "rect 51x1"
                , "rect 1x7"
                , "rotate row y=6 by 1"
                , "rotate column x=50 by 1"
                , "rect " <> show (toInteger (maxBound @Int) + 1) <> "x1"
                ]
                ( Screen.solver
                    >>> isLeft
                    >>> assertBool "invalid screen instruction accepted"
                )
            )
        ]

screenCases :: TestTree
screenCases =
    testGroup
        "screen font and rotations"
        [ testCase "transcribed glyphs" (for_ glyphs checkGlyph)
        , testCase "non-letter display uses placeholder" $
            fmap render (Screen.solver "rect 1x1") @?= Right "1\n0\n"
        , testCase "arbitrarily large rotations wrap" $ do
            let amount = 50 * (toInteger (maxBound @Int) + 1)
            fmap
                render
                ( Screen.solver
                    ( "rect 1x1\nrotate row y=0 by "
                        <> show amount
                        <> "\nrotate column x=0 by "
                        <> show (6 * amount)
                    )
                )
                @?= fmap render (Screen.solver "rect 1x1")
        ]
  where
    checkGlyph :: (Char, [Text]) -> IO ()
    checkGlyph (letter, rows) = do
        let pixels =
                [ (offset + x, y)
                | offset <- [0, 5 .. 45]
                , (y, row) <- zip [0 ..] rows
                , (x, pixel) <- zip [0 ..] (toString row)
                , pixel == '#'
                ]
        fmap render (Screen.solver (paint pixels))
            @?= Right
                ( show (foldl' (\n _ -> n + 1) (0 :: Int) pixels)
                    <> "\n"
                    <> toText (replicate 10 letter)
                    <> "\n"
                )

    glyphs :: [(Char, [Text])]
    glyphs =
        [ ('A', [".##..", "#..#.", "#..#.", "####.", "#..#.", "#..#."])
        , ('B', ["###..", "#..#.", "###..", "#..#.", "#..#.", "###.."])
        , ('E', ["####.", "#....", "###..", "#....", "#....", "####."])
        , ('F', ["####.", "#....", "###..", "#....", "#....", "#...."])
        , ('G', [".##..", "#..#.", "#....", "#.##.", "#..#.", ".###."])
        , ('O', [".##..", "#..#.", "#..#.", "#..#.", "#..#.", ".##.."])
        , ('S', [".###.", "#....", "#....", ".##..", "...#.", "###.."])
        , ('U', ["#..#.", "#..#.", "#..#.", "#..#.", "#..#.", ".##.."])
        ]

-- Conjugate a one-pixel rectangle by reversible row/column rotations,
-- restoring existing pixels while setting precisely the requested coordinate.
paint :: [(Int, Int)] -> Text
paint pixels =
    pixels
        & concatMap setPixel
        & unlines
  where
    setPixel :: (Int, Int) -> [Text]
    setPixel (x, y) =
        [ "rotate column x=" <> show x <> " by " <> show (6 - y)
        , "rotate row y=0 by " <> show (50 - x)
        , "rect 1x1"
        , "rotate row y=0 by " <> show x
        , "rotate column x=" <> show x <> " by " <> show y
        ]
