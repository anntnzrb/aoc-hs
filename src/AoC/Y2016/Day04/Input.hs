-- | Room names contain nonempty lowercase words; sectors are nonnegative.
module AoC.Y2016.Day04.Input (Room (Room, words, sector, checksum), parseInput) where

import Data.Char (isAsciiLower)
import Text.Megaparsec (Parsec, count, eof, errorBundlePretty, parse, satisfy, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Room = Room
    { words :: NonEmpty Text
    , sector :: Natural
    , checksum :: Text
    }

type Parser = Parsec Void Text

parseInput :: Text -> Either Text [Room]
parseInput = parse (room `sepEndBy1` eol <* eof) "rooms" >>> first (toText . errorBundlePretty)

room :: Parser Room
room =
    Room
        <$> ((:|) <$> word <*> many word)
        <*> decimal
        <*> (char '[' *> (toText <$> count 5 (satisfy lowercase)) <* char ']')

lowercase :: Char -> Bool
lowercase letter = isAsciiLower letter

word :: Parser Text
word = takeWhile1P (Just "lowercase word") lowercase <* char '-'
