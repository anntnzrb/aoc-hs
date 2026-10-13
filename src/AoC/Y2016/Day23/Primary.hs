{- |
approach: Interpret jumps and toggles, collapsing verified terminating addition and multiplication loops in the live program.
cost: O(n + s log n) time and O(n) memory for n instructions and s interpreted or collapsed steps across both runs; one parse pass and one indexing pass, with arbitrary-precision arithmetic costs additional.
trade: Runtime pattern checks avoid enormous repeated increments while preserving toggles, register aliasing and signed counters; unmatched loops retain direct execution.
-}
module AoC.Y2016.Day23.Primary (solver) where

import AoC.Assembunny (
    Instruction (Copy, Decrement, Increment, Jump, Output, Toggle),
    Machine (counter, program, registers),
    Register (A),
    Value (Literal, RegisterValue),
    initialise,
    readValue,
 )
import AoC.Assembunny qualified as Assembunny
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day23.Input (Input (Input), parseInput)
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    pure (Answers (show (execute instructions 7)) (Just (show (execute instructions 12))))

execute :: [Instruction] -> Integer -> Integer
execute instructions initial =
    initialise instructions [(A, initial)]
        & iterate step
        & find (\machine -> Map.notMember machine.counter machine.program)
        & maybe 0 (readValue (RegisterValue A))

step :: Machine -> Machine
step machine = fromMaybe (Assembunny.step machine) (multiplyLoop machine <|> addLoop machine)

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
    Output{} -> Nothing

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
