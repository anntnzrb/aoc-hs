{- |
approach: Interpret jumps and toggles, collapsing verified terminating addition and multiplication loops in the live program.
cost: O(n + s log n) time and O(n) memory for n instructions and s interpreted or collapsed steps across both runs; one parse pass and one indexing pass, with arbitrary-precision arithmetic costs additional.
trade: Runtime pattern checks avoid enormous repeated increments while preserving toggles, register aliasing and signed counters; unmatched loops retain direct execution.
-}
module AoC.Y2016.Day23.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day23.Input (
    Input (Input),
    Instruction (Copy, Decrement, Increment, Jump, Toggle),
    Register (A),
    Value (Literal, RegisterValue),
    parseInput,
 )
import Data.Map.Strict qualified as Map

data Machine = Machine
    { counter :: Integer
    , registers :: Map Register Integer
    , program :: Map Integer Instruction
    }

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    pure (Answers (show (execute instructions 7)) (Just (show (execute instructions 12))))

execute :: [Instruction] -> Integer -> Integer
execute instructions initial =
    Machine 0 (one (A, initial)) (Map.fromDistinctAscList (zip [0 ..] instructions))
        & iterate step
        & find (\machine -> Map.notMember machine.counter machine.program)
        & maybe 0 (readValue (RegisterValue A))

step :: Machine -> Machine
step machine = fromMaybe (maybe machine (apply machine) (Map.lookup machine.counter machine.program)) (multiplyLoop machine <|> addLoop machine)

apply :: Machine -> Instruction -> Machine
apply machine = \case
    Copy source destination -> advance (writeValue destination (readValue source machine) machine)
    Increment destination -> advance (writeValue destination (readValue destination machine + 1) machine)
    Decrement destination -> advance (writeValue destination (readValue destination machine - 1) machine)
    Jump condition offset
        | readValue condition machine /= 0 -> machine{counter = machine.counter + readValue offset machine}
        | otherwise -> advance machine
    Toggle offset -> advance machine{program = Map.adjust toggle (machine.counter + readValue offset machine) machine.program}

advance :: Machine -> Machine
advance machine = machine{counter = machine.counter + 1}

toggle :: Instruction -> Instruction
toggle = \case
    Increment value -> Decrement value
    Decrement value -> Increment value
    Toggle value -> Increment value
    Jump source destination -> Copy source destination
    Copy source destination -> Jump source destination

readValue :: Value -> Machine -> Integer
readValue value machine = case value of
    Literal number -> number
    RegisterValue register -> Map.findWithDefault 0 register machine.registers

writeValue :: Value -> Integer -> Machine -> Machine
writeValue destination number machine = case destination of
    Literal _ -> machine
    RegisterValue register -> machine{registers = Map.insert register number machine.registers}

-- Match the live instructions each time: a previous toggle can change any loop.
multiplyLoop :: Machine -> Maybe Machine
multiplyLoop machine = do
    [Copy source (RegisterValue temporary), targetStep, temporaryStep, Jump (RegisterValue inner) (Literal (-2)), outerStep, Jump (RegisterValue outer) (Literal (-5))] <- window 6 machine
    (target, targetDelta) <- adjustment targetStep
    (temporaryRegister, temporaryDelta) <- adjustment temporaryStep
    (outerRegister, outerDelta) <- adjustment outerStep
    guard (temporary == inner && temporary == temporaryRegister && outer == outerRegister)
    guard (target /= temporary && target /= outer && temporary /= outer)
    guard (independent source [target, temporary, outer])
    let multiplicand = readValue source machine
        multiplier = readValue (RegisterValue outer) machine
    guard (multiplicand * temporaryDelta < 0 && multiplier * outerDelta < 0)
    pure (complete machine 6 [(target, readValue (RegisterValue target) machine + targetDelta * abs multiplicand * abs multiplier), (temporary, 0), (outer, 0)])

addLoop :: Machine -> Maybe Machine
addLoop machine = do
    [targetStep, counterStep, Jump (RegisterValue condition) (Literal (-2))] <- window 3 machine
    (target, targetDelta) <- adjustment targetStep
    (counterRegister, counterDelta) <- adjustment counterStep
    let count = readValue (RegisterValue counterRegister) machine
    guard (condition == counterRegister && target /= counterRegister && count * counterDelta < 0)
    pure (complete machine 3 [(target, readValue (RegisterValue target) machine + targetDelta * abs count), (counterRegister, 0)])

window :: Integer -> Machine -> Maybe [Instruction]
window size machine = traverse (\offset -> Map.lookup (machine.counter + offset) machine.program) [0 .. size - 1]

adjustment :: Instruction -> Maybe (Register, Integer)
adjustment = \case
    Increment (RegisterValue register) -> Just (register, 1)
    Decrement (RegisterValue register) -> Just (register, -1)
    Increment (Literal _) -> Nothing
    Decrement (Literal _) -> Nothing
    Copy{} -> Nothing
    Jump{} -> Nothing
    Toggle{} -> Nothing

independent :: Value -> [Register] -> Bool
independent source modified = case source of
    Literal _ -> True
    RegisterValue register -> register `notElem` modified

complete :: Machine -> Integer -> [(Register, Integer)] -> Machine
complete machine width changes =
    machine
        { counter = machine.counter + width
        , registers = foldl' (\registers (register, number) -> Map.insert register number registers) machine.registers changes
        }
