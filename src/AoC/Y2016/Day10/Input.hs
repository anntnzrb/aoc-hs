-- | Nonnegative chip values and bot/bin identifiers in routing instructions.
module AoC.Y2016.Day10.Input (
    Bot (Bot),
    Chip (Chip),
    Destination (ToBot, Output),
    Instruction (Give, Route),
    Input (Input),
    parseInput,
) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

newtype Bot = Bot Natural
    deriving newtype (Eq, Ord, Show)

newtype Chip = Chip Natural
    deriving newtype (Eq, Ord)

data Destination = ToBot Bot | Output Natural
    deriving stock (Eq, Ord)

data Instruction = Give Chip Bot | Route Bot Destination Destination

newtype Input = Input [Instruction]

parseInput :: Text -> Either Text Input
parseInput = parseText (Input <$> instruction `sepEndBy1` eol)

instruction :: Parser Instruction
instruction =
    (Give <$> (string "value " *> (Chip <$> Lexer.decimal)) <*> (string " goes to bot " *> (Bot <$> Lexer.decimal)))
        <|> ( Route
                <$> (string "bot " *> (Bot <$> Lexer.decimal))
                <*> (string " gives low to " *> destination)
                <*> (string " and high to " *> destination)
            )

destination :: Parser Destination
destination =
    (ToBot . Bot <$> (string "bot " *> Lexer.decimal))
        <|> (Output <$> (string "output " *> Lexer.decimal))
