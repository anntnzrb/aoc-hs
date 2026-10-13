-- | Room names contain nonempty lowercase words; sectors are nonnegative.
module AoC.Y2016.Day04.Input (Room (Room, words, sector, checksum), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (count, satisfy, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (decimal)

{- | An encrypted room claim. Parsing enforces lowercase words and a five-letter
checksum; the solver checks whether that checksum is correct. Direct construction
must preserve the grammar invariants.
-}
data Room
    = -- | Name words, sector identifier and claimed checksum.
      Room
      { words :: NonEmpty Text
    -- ^ Nonempty lowercase name words in written order.
      , sector :: Natural
    -- ^ Nonnegative sector identifier and rotation amount.
      , checksum :: Text
    -- ^ Five lowercase letters claimed as the frequency checksum.
      }

{- | Accept hyphenated lowercase names, a nonnegative sector and five-letter bracketed
checksum; reject other syntax. Errors are returned as text.
-}
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
