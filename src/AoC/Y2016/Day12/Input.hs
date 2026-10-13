-- | Assembunny instructions: four registers, integer values and relative jumps.
module AoC.Y2016.Day12.Input (
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    Instruction (Copy, Increment, Decrement, Jump),
    Input (Input),
    parseInput,
) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Register = A | B | C | D

data Value = Literal Integer | RegisterValue Register

data Instruction
    = Copy Value Register
    | Increment Register
    | Decrement Register
    | Jump Value Integer

newtype Input = Input [Instruction]

type Parser = Parsec Void Text

parseInput :: Text -> Either Text Input
parseInput = parse (Input <$> instruction `sepEndBy1` eol <* eof) "assembunny" >>> first (toText . errorBundlePretty)

instruction :: Parser Instruction
instruction =
    (Copy <$> (string "cpy " *> value) <*> (char ' ' *> register))
        <|> (Increment <$> (string "inc " *> register))
        <|> (Decrement <$> (string "dec " *> register))
        <|> (Jump <$> (string "jnz " *> value) <*> (char ' ' *> integer))

value :: Parser Value
value = (Literal <$> integer) <|> (RegisterValue <$> register)

integer :: Parser Integer
integer = Lexer.signed pass Lexer.decimal

register :: Parser Register
register = (A <$ char 'a') <|> (B <$ char 'b') <|> (C <$ char 'c') <|> (D <$ char 'd')
