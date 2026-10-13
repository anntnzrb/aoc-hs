module AoC.AssembunnyTests (tests) where

import AoC.Assembunny (
    Dialect (Basic, Clock, SelfModifying),
    Instruction (Copy, Decrement, Increment, Jump, Output, Toggle),
    Machine (counter, program),
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    acceleratedStep,
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
tests = testGroup "assembunny" [parsing, execution, toggling, acceleration]

acceleration :: TestTree
acceleration = testGroup "loop acceleration" [addition, multiplication, division, refusal]

division :: TestTree
division = testCase "countdown division matches direct execution including the final partial block" $ do
    for_ [(dividendValue, divisor) | dividendValue <- [0 .. 12], divisor <- [1 .. 5]] $ \(dividendValue, divisor) -> do
        let instructions = [Copy (Literal 0) (RegisterValue A), Copy (Literal divisor) (RegisterValue C), Jump (RegisterValue B) (Literal 2), Jump (Literal 1) (Literal 6), Decrement (RegisterValue B), Decrement (RegisterValue C), Jump (RegisterValue C) (Literal (-4)), Increment (RegisterValue A), Jump (Literal 1) (Literal (-7)), Output (RegisterValue A)]
            machine = initialise instructions [(A, 9), (B, dividendValue), (C, -1)]
            steps = 4 + 4 * dividendValue + 3 * (dividendValue `div` divisor)
        acceleratedStep machine @?= direct steps machine

addition :: TestTree
addition = testCase "signed counters and either adjustment order match direct execution" $ do
    for_ [(count, delta, (targetDelta, swapped)) | count <- [-4 .. 4], delta <- [-1, 1], count * delta < 0, targetDelta <- [-1, 1], swapped <- [False, True]] $ \(count, delta, (targetDelta, swapped)) -> do
        let pair = if swapped then [adjust delta B, adjust targetDelta A] else [adjust targetDelta A, adjust delta B]
            instructions = pair <> [Jump (RegisterValue B) (Literal (-2)), Output (RegisterValue A)]
            machine = initialise instructions [(A, 7), (B, count)]
        acceleratedStep machine @?= direct (3 * abs count) machine

multiplication :: TestTree
multiplication = testCase "signed multiplication preserves the complete machine" $ do
    for_ [(inner, outer, targetDelta) | inner <- [-3, 2], outer <- [-2, 3], targetDelta <- [-1, 1]] $ \(inner, outer, targetDelta) -> do
        let innerDelta = negate (signum inner)
            outerDelta = negate (signum outer)
            instructions = [Copy (RegisterValue D) (RegisterValue B), adjust targetDelta A, adjust innerDelta B, Jump (RegisterValue B) (Literal (-2)), adjust outerDelta C, Jump (RegisterValue C) (Literal (-5))]
            machine = initialise instructions [(A, 4), (C, outer), (D, inner)]
        acceleratedStep machine @?= direct (abs outer * (3 * abs inner + 3)) machine

refusal :: TestTree
refusal = testCase "nonterminating, aliased and toggled loops retain direct semantics" $ do
    for_ [0, -2] $ \count -> do
        let machine = initialise [Increment (RegisterValue A), Decrement (RegisterValue B), Jump (RegisterValue B) (Literal (-2))] [(B, count)]
        acceleratedStep machine @?= step machine
    let aliased = initialise [Increment (RegisterValue A), Decrement (RegisterValue A), Jump (RegisterValue A) (Literal (-2))] [(A, 2)]
        dependent = initialise [Copy (RegisterValue A) (RegisterValue B), Increment (RegisterValue A), Decrement (RegisterValue B), Jump (RegisterValue B) (Literal (-2)), Decrement (RegisterValue C), Jump (RegisterValue C) (Literal (-5))] [(A, 2), (C, 3)]
        toggled = initialise [Toggle (Literal 2), Increment (RegisterValue A), Decrement (RegisterValue B), Jump (RegisterValue B) (Literal (-2))] [(B, 2)] & step
    for_ [aliased, dependent, toggled] $ \machine -> acceleratedStep machine @?= step machine

adjust :: Integer -> Register -> Instruction
adjust delta name = (if delta > 0 then Increment else Decrement) (RegisterValue name)

direct :: Integer -> Machine -> Machine
direct count machine = foldl' (\machineState _ -> step machineState) machine [1 .. count]

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
