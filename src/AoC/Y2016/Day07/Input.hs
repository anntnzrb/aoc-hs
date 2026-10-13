-- | IPv7 addresses alternate nonempty lowercase sequences outside and inside brackets.
module AoC.Y2016.Day07.Input (Address (Address, supernets, hypernets), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)

{- | Outside and inside bracket segments of one IPv7 address, each in written order.
Parsing requires nonempty lowercase segments; direct construction must preserve
those invariants.
-}
data Address
    = -- | Outside-bracket segments followed by inside-bracket segments.
      Address
      { supernets :: NonEmpty Text
    -- ^ Nonempty outside-bracket segments in written order.
      , hypernets :: [Text]
    -- ^ Inside-bracket segments in written order, possibly none.
      }

{- | Accept nonempty lowercase IPv7 lines with alternating bracketed segments; reject
empty segments or unbalanced brackets. Errors are returned as text.
-}
parseInput :: Text -> Either Text [Address]
parseInput = parseText (address `sepEndBy1` eol)

address :: Parser Address
address = do
    start <- letters
    rest <- many ((,) <$> (char '[' *> letters <* char ']') <*> letters)
    pure (Address (start :| map snd rest) (map fst rest))

letters :: Parser Text
letters = takeWhile1P (Just "lowercase letters") isAsciiLower
