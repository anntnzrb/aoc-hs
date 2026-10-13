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

-- | The four integer registers available to every dialect.
data Register
    = -- | Register a.
      A
    | -- | Register b.
      B
    | -- | Register c.
      C
    | -- | Register d.
      D
    deriving stock (Eq, Ord, Show)

-- | An immediate integer or a register read at execution time.
data Value
    = -- | An immediate integer.
      Literal Integer
    | -- | Read the named register.
      RegisterValue Register
    deriving stock (Eq, Ord, Show)

-- | One live instruction; literal write destinations are no-ops.
data Instruction
    = -- | Copy the source into the destination.
      Copy Value Value
    | -- | Add one to the destination.
      Increment Value
    | -- | Subtract one from the destination.
      Decrement Value
    | -- | If the first operand is nonzero, jump by the second operand relative to the current counter.
      Jump Value Value
    | -- | Toggle the instruction at the current counter plus the operand.
      Toggle Value
    | -- | Emit the operand value; callers consume it before stepping.
      Output Value
    deriving stock (Eq, Ord, Show)

-- | The instruction grammar accepted before execution.
data Dialect
    = -- | Copy, increment, decrement and jumps with restricted operands.
      Basic
    | -- | Basic operations with general operands and toggle.
      SelfModifying
    | -- | Self-modifying operations plus output.
      Clock
    deriving stock (Eq)

-- | A live program and register state at an integer instruction address.
data Machine
    = Machine
    { counter :: Integer
    -- ^ Current zero-based instruction address; absent addresses halt.
    , registers :: Map Register Integer
    -- ^ Current integer values; missing registers read as zero.
    , program :: Map Integer Instruction
    -- ^ Live address-to-instruction map, including any toggles.
    }
    deriving stock (Eq, Ord, Show)

type Parser = Parsec Void Text

{- | Consume nonempty instruction lines with optional final line ending; reject operands
or opcodes outside the selected dialect with textual errors.
-}
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

{- | Assign addresses from zero and start there; duplicate register assignments keep the
last value, absent registers read as zero.
-}
initialise :: [Instruction] -> [(Register, Integer)] -> Machine
initialise instructions assignments = Machine 0 (Map.fromList assignments) (Map.fromDistinctAscList (zip [0 ..] instructions))

-- | Look up the live instruction at the counter; Nothing means the machine has halted.
current :: Machine -> Maybe Instruction
current machine = Map.lookup machine.counter machine.program

{- | Execute one live instruction, leaving halted machines unchanged; output advances
without retaining emitted values.
-}
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

-- | Resolve a literal or read a register, defaulting an absent register to zero.
readValue :: Value -> Machine -> Integer
readValue operand machine = case operand of
    Literal number -> number
    RegisterValue name -> Map.findWithDefault 0 name machine.registers

writeValue :: Value -> Integer -> Machine -> Machine
writeValue destination number machine = case destination of
    Literal _ -> machine
    RegisterValue name -> machine{registers = Map.insert name number machine.registers}

{- | Perform one step or collapse a verified terminating output-free arithmetic loop;
preserve the exact resulting machine.
-}
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
    -- A zero dividend exits before any decrement, preserving its absence.
    pure (complete machine 9 ([(quotient, result), (temporary, divisor - remainder)] <> [(dividend, 0) | dividendValue > 0]))

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
    pure $
        complete
            machine
            6
            [ (target, readValue (RegisterValue target) machine + targetDelta * abs multiplicand * abs multiplier)
            , (temporary, 0)
            , (outer, 0)
            ]

addLoop :: Machine -> Maybe Machine
addLoop machine = do
    [firstStep, secondStep, Jump (RegisterValue condition) (Literal (-2))] <- window 3 machine
    (firstRegister, firstDelta) <- adjustment firstStep
    (secondRegister, secondDelta) <- adjustment secondStep
    guard (firstRegister /= secondRegister)
    ((target, targetDelta), (counterRegister, counterDelta)) <- roles condition (firstRegister, firstDelta) (secondRegister, secondDelta)
    let count = readValue (RegisterValue counterRegister) machine
    guard (count * counterDelta < 0)
    pure $
        complete
            machine
            3
            [ (target, readValue (RegisterValue target) machine + targetDelta * abs count)
            , (counterRegister, 0)
            ]
  where
    roles :: Register -> (Register, Integer) -> (Register, Integer) -> Maybe ((Register, Integer), (Register, Integer))
    roles condition firstChange secondChange
        | condition == fst secondChange = Just (firstChange, secondChange)
        | condition == fst firstChange = Just (secondChange, firstChange)
        | otherwise = Nothing

window :: Integer -> Machine -> Maybe [Instruction]
window size machine = traverse (\offset -> Map.lookup (machine.counter + offset) machine.program) [0 .. size - 1]

adjustment :: Instruction -> Maybe (Register, Integer)
adjustment = \case
    Increment (RegisterValue registerName) -> Just (registerName, 1)
    Decrement (RegisterValue registerName) -> Just (registerName, -1)
    Increment (Literal _) -> Nothing
    Decrement (Literal _) -> Nothing
    Copy{} -> Nothing
    Jump{} -> Nothing
    Toggle{} -> Nothing
    Output{} -> Nothing

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
