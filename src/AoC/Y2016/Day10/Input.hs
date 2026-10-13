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

-- | A nonnegative routing-bot identifier.
newtype Bot
    = -- | Bot identifier.
      Bot Natural
    deriving newtype (Eq, Ord, Show)

-- | A nonnegative microchip value, ordered numerically.
newtype Chip
    = -- | Numeric chip value.
      Chip Natural
    deriving newtype (Eq, Ord)

-- | Where one routed chip goes: a bot or an output bin.
data Destination
    = -- | Deliver to the named bot.
      ToBot Bot
    | -- | Deliver to the numbered output bin.
      Output Natural
    deriving stock (Eq, Ord)

-- | An initial chip delivery or a persistent low/high routing rule.
data Instruction
    = -- | Give a chip to a bot initially.
      Give Chip Bot
    | -- | Bot, low-chip destination, then high-chip destination.
      Route Bot Destination Destination

-- | Initial chip assignments and routing rules in written order.
newtype Input
    = -- | Routing instructions in written order.
      Input [Instruction]

{- | Accept chip assignments and low/high routing lines with nonnegative identifiers;
reject other instruction syntax. Errors are returned as text.
-}
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
