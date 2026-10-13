module AoC.AssembunnyTests (tests) where

import AoC.Assembunny (
    Dialect (Basic, Clock, SelfModifying),
    Instruction (Copy, Decrement, Increment, Jump, Output, Toggle),
    Machine (counter, program),
    Register (A, B),
    Value (Literal, RegisterValue),
    current,
    initialise,
    parseProgram,
    readValue,
    step,
 )
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

tests :: TestTree
tests = testGroup "assembunny" [parsing, execution, toggling]

parsing :: TestTree
parsing = testCase "dialects preserve operand restrictions and consume all input" $ do
    parseProgram Basic "cpy -2 a\njnz a -1\n" @?= Right [Copy (Literal (-2)) (RegisterValue A), Jump (RegisterValue A) (Literal (-1))]
    for_ ["cpy 1 2", "inc 2", "jnz 1 a", "tgl a", "out a", "inc e", "inc a\n\n", ""] $ \text ->
        assertBool "invalid basic program" (isLeft (parseProgram Basic text))
    parseProgram SelfModifying "cpy 1 2\ntgl a" @?= Right [Copy (Literal 1) (Literal 2), Toggle (RegisterValue A)]
    assertBool "output requires clock dialect" (isLeft (parseProgram SelfModifying "out a"))
    parseProgram Clock "out -1\n" @?= Right [Output (Literal (-1))]

execution :: TestTree
execution = testCase "copy, adjustment, jumps, output and halt" $ do
    let instructions = [Copy (Literal 3) (RegisterValue A), Decrement (RegisterValue A), Jump (RegisterValue A) (Literal (-1)), Increment (RegisterValue B), Output (RegisterValue B)]
        states = iterate step (initialise instructions [])
        finished = states & drop 9 & viaNonEmpty head
    fmap (readValue (RegisterValue A)) finished @?= Just 0
    fmap (readValue (RegisterValue B)) finished @?= Just 1
    fmap current finished @?= Just Nothing
    fmap step finished @?= finished
    let invalid = initialise [Copy (Literal 8) (Literal 1), Increment (Literal 3), Decrement (Literal 2)] [(A, 4)] & iterate step & drop 3 & viaNonEmpty head
    fmap (readValue (RegisterValue A)) invalid @?= Just 4
    (initialise [Jump (Literal 1) (RegisterValue A)] [(A, -2)] & step).counter @?= -2
    (initialise [Jump (Literal 1) (Literal 0)] [] & step).counter @?= 0

toggling :: TestTree
toggling = testCase "toggle by arity, including self, output and invalid targets" $ do
    for_ [(Increment (RegisterValue A), Decrement (RegisterValue A)), (Decrement (RegisterValue A), Increment (RegisterValue A)), (Toggle (Literal 2), Increment (Literal 2)), (Output (Literal 3), Increment (Literal 3)), (Jump (Literal 1) (Literal 2), Copy (Literal 1) (Literal 2)), (Copy (Literal 1) (RegisterValue A), Jump (Literal 1) (RegisterValue A))] $ \(before, after) ->
        Map.lookup 1 (step (initialise [Toggle (Literal 1), before] [])).program @?= Just after
    let self = initialise [Toggle (Literal 0)] [] & step
    current self @?= Nothing
    Map.lookup 0 self.program @?= Just (Increment (Literal 0))
    let missing = initialise [Toggle (Literal (-1))] []
    (step missing).program @?= missing.program
