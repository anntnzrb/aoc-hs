-- | IPv7 addresses alternate nonempty lowercase sequences outside and inside brackets.
module AoC.Y2016.Day07.Input (Address (Address, supernets, hypernets), parseInput) where

import Data.Char (isAsciiLower)
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepEndBy1, takeWhile1P)
import Text.Megaparsec.Char (char, eol)

data Address = Address
    { supernets :: NonEmpty Text
    , hypernets :: [Text]
    }

type Parser = Parsec Void Text

parseInput :: Text -> Either Text [Address]
parseInput = parse (address `sepEndBy1` eol <* eof) "IPv7" >>> first (toText . errorBundlePretty)

address :: Parser Address
address = do
    start <- letters
    rest <- many ((,) <$> (char '[' *> letters <* char ']') <*> letters)
    pure (Address (start :| map snd rest) (map fst rest))

letters :: Parser Text
letters = takeWhile1P (Just "lowercase letters") isAsciiLower
