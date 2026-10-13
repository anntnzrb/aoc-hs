module AoC.Y2016.Day10 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day10.Primary qualified as Bots
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "2016/10"
        [ testGroup
            "terminating chip routing"
            [ testCase "self-loop fails" selfLoop
            , testCase "two-bot cycle fails" twoBotCycle
            , testCase "finite route retains output product" finiteRoute
            ]
        ]

selfLoop :: IO ()
selfLoop =
    [ "value 2 goes to bot 0"
    , "value 3 goes to bot 0"
    , "bot 0 gives low to bot 0 and high to bot 0"
    ]
        & unlines
        & Bots.solver
        & fmap render
        & (@?= Left "Chip routing repeats a configuration without terminating")

twoBotCycle :: IO ()
twoBotCycle =
    [ "value 2 goes to bot 0"
    , "value 3 goes to bot 0"
    , "bot 0 gives low to bot 1 and high to bot 1"
    , "bot 1 gives low to bot 0 and high to bot 0"
    ]
        & unlines
        & Bots.solver
        & fmap render
        & (@?= Left "Chip routing repeats a configuration without terminating")

finiteRoute :: IO ()
finiteRoute =
    [ "value 5 goes to bot 2"
    , "bot 2 gives low to bot 1 and high to bot 0"
    , "value 3 goes to bot 1"
    , "bot 1 gives low to output 1 and high to bot 0"
    , "bot 0 gives low to output 2 and high to output 0"
    , "value 2 goes to bot 2"
    ]
        & unlines
        & Bots.solver
        & fmap render
        & (@?= Right "0\n30\n")
