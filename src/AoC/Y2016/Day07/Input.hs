-- | IPv7 addresses alternate nonempty lowercase sequences outside and inside brackets.
module AoC.Y2016.Day07.Input (Address (Address, supernets, hypernets), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Char (isAsciiLower)
import Text.Megaparsec (sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)

data Address = Address
    { supernets :: NonEmpty Text
    , hypernets :: [Text]
    }

parseInput :: Text -> Either Text [Address]
parseInput = parseText (address `sepEndBy1` eol)

address :: Parser Address
address = do
    start <- letters
    rest <- many ((,) <$> (char '[' *> letters <* char ']') <*> letters)
    pure (Address (start :| map snd rest) (map fst rest))

letters :: Parser Text
letters = takeWhile1P (Just "lowercase letters") isAsciiLower
