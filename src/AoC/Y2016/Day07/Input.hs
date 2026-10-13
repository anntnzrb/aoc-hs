-- | IPv7 addresses contain nonempty character sequences outside or inside brackets.
module AoC.Y2016.Day07.Input (Address (Address), Segment (Supernet, Hypernet), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)

{- | Nonempty segments of one IPv7 address in written order. The statement defines
segments by bracket containment, without requiring outside text between hypernets:
leading, terminal and adjacent hypernets are allowed. Adjacent outside characters
form one maximal supernet. Segment text excludes square brackets, CR and LF;
parsing enforces nonempty segments and balanced, non-nested brackets. Direct
construction must preserve these invariants.
-}
newtype Address
    = -- | At least one outside or inside segment.
      Address (NonEmpty Segment)

-- | A character sequence classified by its bracket containment.
data Segment
    = -- | Nonempty characters outside square brackets.
      Supernet Text
    | -- | Nonempty characters enclosed by one pair of square brackets.
      Hypernet Text

{- | Accept nonempty IPv7 lines containing supernets and bracketed hypernets in any
order. Segment characters exclude square brackets, CR and LF; reject empty,
unbalanced or nested bracketed segments. Errors are returned as text.
-}
parseInput :: Text -> Either Text [Address]
parseInput = parseText (address `sepEndBy1` eol)

address :: Parser Address
address = Address <$> ((:|) <$> segment <*> many segment)

segment :: Parser Segment
segment = Supernet <$> characters <|> Hypernet <$> (char '[' *> characters <* char ']')

characters :: Parser Text
characters = takeWhile1P (Just "segment characters") (\c -> c /= '[' && c /= ']' && c /= '\r' && c /= '\n')
