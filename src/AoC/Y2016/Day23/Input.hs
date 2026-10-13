-- | Self-modifying assembunny: operands survive toggles, including invalid destinations.
module AoC.Y2016.Day23.Input (
    Register (A, B, C, D),
    Value (Literal, RegisterValue),
    Instruction (Copy, Increment, Decrement, Jump, Toggle),
    Input (Input),
    parseInput,
) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1)
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Register = A | B | C | D
    deriving stock (Eq, Ord)

data Value = Literal Integer | RegisterValue Register

data Instruction
    = Copy Value Value
    | Increment Value
    | Decrement Value
    | Jump Value Value
    | Toggle Value

newtype Input = Input [Instruction]

type Parser = Parsec Void Text

parseInput :: Text -> Either Text Input
parseInput = parse (Input <$> instruction `sepEndBy1` eol <* eof) "assembunny" >>> first (toText . errorBundlePretty)

instruction :: Parser Instruction
instruction =
    (Copy <$> (string "cpy " *> value) <*> (char ' ' *> value))
        <|> (Increment <$> (string "inc " *> value))
        <|> (Decrement <$> (string "dec " *> value))
        <|> (Jump <$> (string "jnz " *> value) <*> (char ' ' *> value))
        <|> (Toggle <$> (string "tgl " *> value))

value :: Parser Value
value = (Literal <$> Lexer.signed pass Lexer.decimal) <|> (RegisterValue <$> register)

register :: Parser Register
register = (A <$ char 'a') <|> (B <$ char 'b') <|> (C <$ char 'c') <|> (D <$ char 'd')
