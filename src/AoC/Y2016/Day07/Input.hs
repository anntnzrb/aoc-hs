-- | IPv7 addresses contain nonempty lowercase sequences outside or inside brackets.
module AoC.Y2016.Day07.Input (Address (Address), Segment (Supernet, Hypernet), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)

{- | Nonempty segments of one IPv7 address in written order. The statement defines
segments by bracket containment, without requiring outside text between hypernets:
leading, terminal and adjacent hypernets are allowed. Adjacent outside letters
form one maximal supernet. Parsing enforces nonempty lowercase segment text and
balanced, non-nested brackets; direct construction must preserve these invariants.
-}
newtype Address
    = -- | At least one outside or inside segment.
      Address (NonEmpty Segment)

-- | A lowercase sequence classified by its bracket containment.
data Segment
    = -- | Nonempty letters outside square brackets.
      Supernet Text
    | -- | Nonempty letters enclosed by one pair of square brackets.
      Hypernet Text

{- | Accept nonempty IPv7 lines containing lowercase supernets and bracketed
hypernets in any order; reject empty, unbalanced or nested bracketed segments.
Errors are returned as text.
-}
parseInput :: Text -> Either Text [Address]
parseInput = parseText (address `sepEndBy1` eol)

address :: Parser Address
address = Address <$> ((:|) <$> segment <*> many segment)

segment :: Parser Segment
segment = Supernet <$> letters <|> Hypernet <$> (char '[' *> letters <* char ']')

letters :: Parser Text
letters = takeWhile1P (Just "lowercase letters") isAsciiLower
