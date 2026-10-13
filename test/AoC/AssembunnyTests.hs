module AoC.AssembunnyTests (tests) where

import AoC.Assembunny (
    Dialect (Basic, Clock, SelfModifying),
    Instruction (Copy, Decrement, Increment, Jump, Output, Toggle),
    Machine (counter, program, registers),
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
import Hedgehog (Gen, Property, PropertyT, cover, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests = testGroup "assembunny" [parsing, execution, toggling, acceleration]

acceleration :: TestTree
acceleration = testGroup "loop acceleration" [addition, multiplication, division, sparseDivision, refusal, testProperty "bounded differential execution" differential]

sparseDivision :: TestTree
sparseDivision = testCase "zero division preserves an absent dividend register" $ do
    let instructions = [Copy (Literal 0) (RegisterValue A), Copy (Literal 2) (RegisterValue C), Jump (RegisterValue B) (Literal 2), Jump (Literal 1) (Literal 6), Decrement (RegisterValue B), Decrement (RegisterValue C), Jump (RegisterValue C) (Literal (-4)), Increment (RegisterValue A), Jump (Literal 1) (Literal (-7))]
        machine = initialise instructions []
    acceleratedStep machine @?= direct 4 machine

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

differential :: Property
differential = withTests 600 (property differentialExecution)

differentialExecution :: PropertyT IO ()
differentialExecution = do
    (patternName, mutation, machine) <- forAll genMachine
    let literal = boundedRun step machine
    cover 20 "addition" (patternName == Addition)
    cover 20 "multiplication" (patternName == Multiplication)
    cover 20 "division" (patternName == Division)
    cover 10 "exact pattern" (mutation == Exact)
    cover 10 "perturbed instruction" (mutation == Perturbed)
    cover 10 "aliased registers" (mutation == Aliased)
    cover 10 "register jump offsets" (mutation == RegisterOffset)
    cover 10 "wrong adjustment signs" (mutation == WrongSigns)
    cover 20 "sparse assignments" (Map.size machine.registers < 4)
    cover 20 "literal halts" (isJust literal)
    for_ literal $ \result -> boundedRun acceleratedStep machine === Just result

-- The cap applies independently to each interpreter; output is observed before
-- every transition, just as callers of the shared machine consume it.
boundedRun :: (Machine -> Machine) -> Machine -> Maybe (Machine, [Integer])
boundedRun transition machine = scanl' next (machine, []) [1 .. 1000 :: Int] & find (isNothing . current . fst)
  where
    next :: (Machine, [Integer]) -> Int -> (Machine, [Integer])
    next (machineState, outputs) _ = (transition machineState, emitted <> outputs)
      where
        emitted = case current machineState of
            Just (Output operand) -> [readValue operand machineState]
            Just Copy{} -> []
            Just Increment{} -> []
            Just Decrement{} -> []
            Just Jump{} -> []
            Just Toggle{} -> []
            Nothing -> []

data LoopPattern = Addition | Multiplication | Division
    deriving stock (Eq, Show)

data Mutation = Exact | Perturbed | Aliased | RegisterOffset | WrongSigns
    deriving stock (Eq, Show)

-- Ordered, distinct register roles, renamed together before perturbation.
data Roles = Roles Register Register Register Register

genMachine :: Gen (LoopPattern, Mutation, Machine)
genMachine = do
    roles <- Gen.element [Roles a b c d | a <- names, b <- names, a /= b, c <- names, a /= c, d <- names, a /= d, b /= c, b /= d, c /= d]
    assignments <- genAssignments
    patternName <- Gen.element [Addition, Multiplication, Division]
    (instructions, seeded) <- case patternName of
        Addition -> genAddition roles assignments
        Multiplication -> genMultiplication roles assignments
        Division -> genDivision roles assignments
    mutation <- Gen.element [Exact, Perturbed, Aliased, RegisterOffset, WrongSigns]
    changed <- mutate mutation roles instructions
    keepSeed <- Gen.bool
    let initial = if mutation == Exact || keepSeed then seeded else assignments
        outputs = Output (Literal 17) : map (Output . RegisterValue) names
    pure (patternName, mutation, initialise (Output (Literal (-17)) : changed <> outputs) initial)
  where
    names = [A, B, C, D]

genAssignments :: Gen [(Register, Integer)]
genAssignments = catMaybes <$> traverse assignment [A, B, C, D]
  where
    assignment :: Register -> Gen (Maybe (Register, Integer))
    assignment name = Gen.choice [pure Nothing, Just . (name,) <$> Gen.integral (Range.linear (-6) 6)]

genAddition :: Roles -> [(Register, Integer)] -> Gen ([Instruction], [(Register, Integer)])
genAddition (Roles target counterName _ _) assignments = do
    count <- Gen.integral (Range.linear 1 6)
    delta <- Gen.element [-1, 1]
    targetDelta <- Gen.element [-1, 1]
    swapped <- Gen.bool
    let pair = if swapped then [adjust delta counterName, adjust targetDelta target] else [adjust targetDelta target, adjust delta counterName]
        instructions = pair <> [Jump (RegisterValue counterName) (Literal (-2))]
    pure (instructions, assignments <> [(counterName, negate delta * count)])

genMultiplication :: Roles -> [(Register, Integer)] -> Gen ([Instruction], [(Register, Integer)])
genMultiplication (Roles target temporary outer sourceName) assignments = do
    innerCount <- Gen.integral (Range.linear 1 6)
    outerCount <- Gen.integral (Range.linear 1 6)
    innerDelta <- Gen.element [-1, 1]
    outerDelta <- Gen.element [-1, 1]
    targetDelta <- Gen.element [-1, 1]
    source <- Gen.element [Literal (negate innerDelta * innerCount), RegisterValue sourceName]
    let instructions = [Copy source (RegisterValue temporary), adjust targetDelta target, adjust innerDelta temporary, Jump (RegisterValue temporary) (Literal (-2)), adjust outerDelta outer, Jump (RegisterValue outer) (Literal (-5))]
    pure (instructions, assignments <> [(outer, negate outerDelta * outerCount)] <> [(sourceName, negate innerDelta * innerCount) | source == RegisterValue sourceName])

genDivision :: Roles -> [(Register, Integer)] -> Gen ([Instruction], [(Register, Integer)])
genDivision (Roles quotient dividend temporary _) assignments = do
    count <- Gen.integral (Range.linear 0 20)
    divisor <- Gen.integral (Range.linear 1 6)
    absent <- Gen.bool
    exitCondition <- Gen.element [-1, 1]
    repeatCondition <- Gen.element [-1, 1]
    let instructions = [Copy (Literal 0) (RegisterValue quotient), Copy (Literal divisor) (RegisterValue temporary), Jump (RegisterValue dividend) (Literal 2), Jump (Literal exitCondition) (Literal 6), Decrement (RegisterValue dividend), Decrement (RegisterValue temporary), Jump (RegisterValue temporary) (Literal (-4)), Increment (RegisterValue quotient), Jump (Literal repeatCondition) (Literal (-7))]
        seeded = filter ((/= dividend) . fst) assignments <> [(dividend, count) | count /= 0 || not absent]
    pure (instructions, seeded)

mutate :: Mutation -> Roles -> [Instruction] -> Gen [Instruction]
mutate mutation (Roles target counterName _ spare) instructions = case mutation of
    Perturbed -> do
        index <- Gen.element (zip [0 ..] instructions <&> fst) :: Gen Int
        replacement <- Gen.filter (`notElem` instructions) genInstruction
        pure [if position == index then replacement else instructionValue | (position, instructionValue) <- zip [0 ..] instructions]
    Aliased -> pure (map (mapOperands alias) instructions)
    RegisterOffset -> pure (map (registerJump spare) instructions)
    WrongSigns -> pure (map reverseAdjustment instructions)
    Exact -> pure instructions
  where
    alias operand = if operand == RegisterValue counterName then RegisterValue target else operand
    reverseAdjustment = \case
        Increment operand -> Decrement operand
        Decrement operand -> Increment operand
        instructionValue -> instructionValue

registerJump :: Register -> Instruction -> Instruction
registerJump spare = \case
    Jump condition _ -> Jump condition (RegisterValue spare)
    instructionValue@Copy{} -> instructionValue
    instructionValue@Increment{} -> instructionValue
    instructionValue@Decrement{} -> instructionValue
    instructionValue@Toggle{} -> instructionValue
    instructionValue@Output{} -> instructionValue

genInstruction :: Gen Instruction
genInstruction = Gen.choice [Copy <$> operand <*> operand, Increment <$> operand, Decrement <$> operand, Jump <$> operand <*> operand, Toggle <$> operand, Output <$> operand]
  where
    operand :: Gen Value
    operand = Gen.choice [Literal <$> Gen.integral (Range.linear (-6) 6), RegisterValue <$> Gen.element [A, B, C, D]]

mapOperands :: (Value -> Value) -> Instruction -> Instruction
mapOperands transform = \case
    Copy source destination -> Copy (transform source) (transform destination)
    Increment operand -> Increment (transform operand)
    Decrement operand -> Decrement (transform operand)
    Jump condition offset -> Jump (transform condition) (transform offset)
    Toggle operand -> Toggle (transform operand)
    Output operand -> Output (transform operand)

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
