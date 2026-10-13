-- | Room names contain nonempty lowercase words; sectors are nonnegative.
module AoC.Y2016.Day04.Input (Room (Room, words, sector, checksum), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (count, satisfy, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

data Room = Room
    { words :: NonEmpty Text
    , sector :: Natural
    , checksum :: Text
    }

parseInput :: Text -> Either Text [Room]
parseInput = parseText (room `sepEndBy1` eol)

room :: Parser Room
room =
    Room
        <$> ((:|) <$> word <*> many word)
        <*> decimal
        <*> (char '[' *> (toText <$> count 5 (satisfy isAsciiLower)) <* char ']')

word :: Parser Text
word = takeWhile1P (Just "lowercase word") isAsciiLower <* char '-'
