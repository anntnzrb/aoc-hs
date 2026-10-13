module AoC.ParseTests (tests) where

import AoC.Parse (boundedDecimal, boundedSignedDecimal, parseText)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "bounded decimal parsing" [naturalTests, signedTests]

naturalTests :: TestTree
naturalTests =
    testGroup
        "natural"
        [ testCase "natural in range and zero" $ do
            parseText (boundedDecimal @Int) "42" @?= Right 42
            parseText (boundedDecimal @Int) "0" @?= Right 0
        , testCase "natural maximum bounds" $ do
            parseText (boundedDecimal @Int) (show (maxBound @Int)) @?= Right maxBound
            parseText (boundedDecimal @Word) (show (maxBound @Word)) @?= Right maxBound
        , testCase "natural overflow is rejected" $ do
            isLeft (parseText (boundedDecimal @Int) (show (toInteger (maxBound @Int) + 1))) @?= True
            isLeft (parseText (boundedDecimal @Word) (show (toInteger (maxBound @Word) + 1))) @?= True
            isLeft (parseText (boundedDecimal @Int) "999999999999999999999999999999999999999999") @?= True
        , testCase "signs, whitespace and incomplete input follow the grammar" $
            for_ ["", "-1", "+1", " 1", "1 ", "1x"] (\text -> isLeft (parseText (boundedDecimal @Int) text) @?= True)
        ]

signedTests :: TestTree
signedTests =
    testGroup
        "signed"
        [ testCase "signed range includes both bounds" $ do
            parseText (boundedSignedDecimal @Int) "-42" @?= Right (-42)
            parseText (boundedSignedDecimal @Int) "+42" @?= Right 42
            parseText (boundedSignedDecimal @Int) (show (minBound @Int)) @?= Right minBound
            parseText (boundedSignedDecimal @Int) (show (maxBound @Int)) @?= Right maxBound
        , testCase "signed overflow and negative unsigned values are rejected" $ do
            isLeft (parseText (boundedSignedDecimal @Int) (show (toInteger (minBound @Int) - 1))) @?= True
            isLeft (parseText (boundedSignedDecimal @Int) (show (toInteger (maxBound @Int) + 1))) @?= True
            isLeft (parseText (boundedSignedDecimal @Word) "-1") @?= True
        , testCase "signs, whitespace and incomplete input follow the grammar" $
            for_ ["", "+", "-", "- 1", "+ 1", "1x"] (\text -> isLeft (parseText (boundedSignedDecimal @Int) text) @?= True)
        ]
