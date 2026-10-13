module AoC.SolutionTests (tests) where

import AoC.Solution (answers, part1, render)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "solution answers"
        [ testCase "one or two non-empty answers render in order" $ do
            fmap render (answers "a" Nothing) @?= Right "a\n"
            fmap render (answers "a" (Just "b")) @?= Right "a\nb\n"
            fmap part1 (answers "a" (Just "b")) @?= Right "a"
        , testCase "valid text is preserved" $
            fmap render (answers "  café  " (Just "0")) @?= Right "  café  \n0\n"
        , testCase "empty answers are rejected in either part" $ do
            isLeft (answers "" Nothing) @?= True
            isLeft (answers "a" (Just "")) @?= True
        , testCase
            "CR and LF are rejected in either part"
            ( for_ ["\n", "\r", "\r\n", "a\nb", "a\rb", "a\n", "\ra"] $ \value -> do
                isLeft (answers value Nothing) @?= True
                isLeft (answers "a" (Just value)) @?= True
            )
        ]
