{- | Checked decimal parsing for bounded integral domain values on Text.
'boundedDecimal' consumes one or more digits without a sign;
'boundedSignedDecimal' also accepts an immediately preceding '+' or '-'.
Neither consumes whitespace. Values outside the target type's inclusive
minimum and maximum fail before conversion, including negative unsigned values.
'parseText' requires complete input consumption and returns textual errors.
-}
module AoC.Parse (Parser, boundedDecimal, boundedSignedDecimal, parseText) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse)
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void Text

boundedDecimal :: (Integral a, Bounded a) => Parser a
boundedDecimal = Lexer.decimal >>= checkedIntegral

boundedSignedDecimal :: (Integral a, Bounded a) => Parser a
boundedSignedDecimal = Lexer.signed pass Lexer.decimal >>= checkedIntegral

checkedIntegral :: forall a. (Integral a, Bounded a) => Integer -> Parser a
checkedIntegral number
    | number < toInteger (minBound @a) || number > toInteger (maxBound @a) = fail "Decimal is outside the target type's bounds"
    | otherwise = pure (fromInteger number)

parseText :: Parser a -> Text -> Either Text a
parseText parser = parse (parser <* eof) "input" >>> first (toText . errorBundlePretty)
