{- | Pure Assembunny parsing and single-instruction execution. Registers a..d
hold arbitrary integers and missing entries read as zero. Program addresses
start at zero; jumps are relative to the current address. An absent address
halts: 'step' then leaves the machine unchanged. Literal destinations are
no-ops, including after toggling. Toggles affect the live program, ignore
absent targets, and turn output into increment. Output advances the counter;
callers inspect 'current' and 'readValue' before stepping to consume it.

'parseProgram' consumes nonempty instruction lines with one optional final
line ending, returning textual errors. Basic accepts register destinations
and literal jump offsets; SelfModifying adds toggle and general operands;
Clock also adds output. Execution policy and termination belong to callers.
'initialise' accepts register assignments (last duplicate wins), at address 0.

'acceleratedStep' executes one instruction or a verified terminating addition,
multiplication or countdown division loop from the live program. Collapsed loops contain neither
output nor toggles, require distinct modified registers and invariant sources,
and move signed counters towards zero. It preserves the exact final machine;
callers needing every intermediate state must use 'step' instead.
-}
module AoC.Assembunny (
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    Instruction (Copy, Increment, Decrement, Jump, Toggle, Output),
    Dialect (Basic, SelfModifying, Clock),
    Machine (counter, registers, program),
    parseProgram,
    initialise,
    current,
    step,
    acceleratedStep,
    readValue,
) where

import Data.Map.Strict qualified as Map
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Register = A | B | C | D
    deriving stock (Eq, Ord, Show)

data Value = Literal Integer | RegisterValue Register
    deriving stock (Eq, Ord, Show)

data Instruction
    = Copy Value Value
    | Increment Value
    | Decrement Value
    | Jump Value Value
    | Toggle Value
    | Output Value
    deriving stock (Eq, Ord, Show)

data Dialect = Basic | SelfModifying | Clock
    deriving stock (Eq)

data Machine = Machine
    { counter :: Integer
    , registers :: Map Register Integer
    , program :: Map Integer Instruction
    }
    deriving stock (Eq, Ord, Show)

type Parser = Parsec Void Text

parseProgram :: Dialect -> Text -> Either Text [Instruction]
parseProgram dialect = parse (instruction dialect `sepEndBy1` eol <* eof) "assembunny" >>> first (toText . errorBundlePretty)

instruction :: Dialect -> Parser Instruction
instruction dialect =
    (Copy <$> (string "cpy " *> value) <*> (char ' ' *> destination))
        <|> (Increment <$> (string "inc " *> destination))
        <|> (Decrement <$> (string "dec " *> destination))
        <|> (Jump <$> (string "jnz " *> value) <*> (char ' ' *> offset))
        <|> (Toggle <$> (guard (dialect /= Basic) *> string "tgl " *> value))
        <|> (Output <$> (guard (dialect == Clock) *> string "out " *> value))
  where
    destination = if dialect == Basic then RegisterValue <$> register else value
    offset = if dialect == Basic then Literal <$> integer else value

value :: Parser Value
value = (Literal <$> integer) <|> (RegisterValue <$> register)

integer :: Parser Integer
integer = Lexer.signed pass Lexer.decimal

register :: Parser Register
register = (A <$ char 'a') <|> (B <$ char 'b') <|> (C <$ char 'c') <|> (D <$ char 'd')

initialise :: [Instruction] -> [(Register, Integer)] -> Machine
initialise instructions assignments = Machine 0 (Map.fromList assignments) (Map.fromDistinctAscList (zip [0 ..] instructions))

current :: Machine -> Maybe Instruction
current machine = Map.lookup machine.counter machine.program

step :: Machine -> Machine
step machine = maybe machine (apply machine) (current machine)

apply :: Machine -> Instruction -> Machine
apply machine = \case
    Copy source destination -> advance (writeValue destination (readValue source machine) machine)
    Increment destination -> advance (writeValue destination (readValue destination machine + 1) machine)
    Decrement destination -> advance (writeValue destination (readValue destination machine - 1) machine)
    Jump condition offset
        | readValue condition machine /= 0 -> machine{counter = machine.counter + readValue offset machine}
        | otherwise -> advance machine
    Toggle offset -> advance machine{program = Map.adjust toggle (machine.counter + readValue offset machine) machine.program}
    Output _ -> advance machine

advance :: Machine -> Machine
advance machine = machine{counter = machine.counter + 1}

toggle :: Instruction -> Instruction
toggle = \case
    Increment operand -> Decrement operand
    Decrement operand -> Increment operand
    Toggle operand -> Increment operand
    Output operand -> Increment operand
    Jump source destination -> Copy source destination
    Copy source destination -> Jump source destination

readValue :: Value -> Machine -> Integer
readValue operand machine = case operand of
    Literal number -> number
    RegisterValue name -> Map.findWithDefault 0 name machine.registers

writeValue :: Value -> Integer -> Machine -> Machine
writeValue destination number machine = case destination of
    Literal _ -> machine
    RegisterValue name -> machine{registers = Map.insert name number machine.registers}

acceleratedStep :: Machine -> Machine
acceleratedStep machine = fromMaybe (step machine) (divideLoop machine <|> multiplyLoop machine <|> addLoop machine)

-- Each complete block consumes divisor units; the last block stops at zero.
divideLoop :: Machine -> Maybe Machine
divideLoop machine = do
    Copy (Literal 0) (RegisterValue quotient) <- current machine
    [_, Copy (Literal divisor) (RegisterValue temporary), Jump (RegisterValue dividend) (Literal 2), Jump (Literal exitCondition) (Literal 6), Decrement (RegisterValue consumed), Decrement (RegisterValue remaining), Jump (RegisterValue inner) (Literal (-4)), Increment (RegisterValue produced), Jump (Literal repeatCondition) (Literal (-7))] <- window 9 machine
    guard (dividend == consumed && temporary == remaining && temporary == inner && quotient == produced)
    guard (quotient /= dividend && quotient /= temporary && dividend /= temporary)
    guard (divisor > 0 && exitCondition /= 0 && repeatCondition /= 0)
    let dividendValue = readValue (RegisterValue dividend) machine
    guard (dividendValue >= 0)
    let (result, remainder) = dividendValue `divMod` divisor
    pure (complete machine 9 [(quotient, result), (dividend, 0), (temporary, divisor - remainder)])

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
    [firstStep, secondStep, Jump (RegisterValue condition) (Literal (-2))] <- window 3 machine
    (firstRegister, firstDelta) <- adjustment firstStep
    (secondRegister, secondDelta) <- adjustment secondStep
    guard (firstRegister /= secondRegister)
    ((target, targetDelta), (counterRegister, counterDelta)) <-
        if condition == secondRegister
            then Just ((firstRegister, firstDelta), (secondRegister, secondDelta))
            else if condition == firstRegister then Just ((secondRegister, secondDelta), (firstRegister, firstDelta)) else Nothing
    let count = readValue (RegisterValue counterRegister) machine
    guard (count * counterDelta < 0)
    pure (complete machine 3 [(target, readValue (RegisterValue target) machine + targetDelta * abs count), (counterRegister, 0)])

window :: Integer -> Machine -> Maybe [Instruction]
window size machine = traverse (\offset -> Map.lookup (machine.counter + offset) machine.program) [0 .. size - 1]

adjustment :: Instruction -> Maybe (Register, Integer)
adjustment = \case
    Increment (RegisterValue registerName) -> Just (registerName, 1)
    Decrement (RegisterValue registerName) -> Just (registerName, -1)
    _ -> Nothing

independent :: Value -> [Register] -> Bool
independent source modified = case source of
    Literal _ -> True
    RegisterValue registerName -> registerName `notElem` modified

complete :: Machine -> Integer -> [(Register, Integer)] -> Machine
complete machine width changes =
    machine
        { counter = machine.counter + width
        , registers = foldl' (\registerValues (registerName, number) -> Map.insert registerName number registerValues) machine.registers changes
        }
