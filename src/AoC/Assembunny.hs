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
-}
module AoC.Assembunny (
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    Instruction (Copy, Increment, Decrement, Jump, Toggle, Output),
    Dialect (Basic, SelfModifying, Clock),
    Machine (Machine, counter, registers, program),
    parseProgram,
    initialise,
    current,
    step,
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
