{- | Checked decimal parsing for bounded integral domain values on Text.
'boundedDecimal' consumes one or more digits without a sign;
'boundedSignedDecimal' also accepts an immediately preceding '+' or '-'.
'boundedSignedDecimalWithin' additionally enforces a supplied inclusive range.
Neither consumes whitespace. Values outside the target type's inclusive
minimum and maximum fail before conversion, including negative unsigned values.
'parseText' requires complete input consumption and returns textual errors.
-}
module AoC.Parse (Parser, boundedDecimal, boundedSignedDecimal, boundedSignedDecimalWithin, parseText) where

import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse)
import Text.Megaparsec.Char.Lexer qualified as Lexer

-- | A Text parser with no custom error component.
type Parser = Parsec Void Text

{- | Consume unsigned decimal digits without whitespace; fail before conversion when the
result exceeds the target bounds.
-}
boundedDecimal :: (Integral a, Bounded a) => Parser a
boundedDecimal = Lexer.decimal >>= checkedIntegral

{- | Consume decimal digits with an optional adjacent sign and no whitespace; fail for
values outside the target bounds.
-}
boundedSignedDecimal :: (Integral a, Bounded a) => Parser a
boundedSignedDecimal = Lexer.signed pass Lexer.decimal >>= checkedIntegral

{- | Consume a checked signed decimal without whitespace, then require it to lie
within the supplied inclusive minimum and maximum. Reversed bounds reject every
value. Machine overflow fails before the range check; range failures report
"out of range".
-}
boundedSignedDecimalWithin :: (Integral a, Bounded a) => (a, a) -> Parser a
boundedSignedDecimalWithin (minimum', maximum') = boundedSignedDecimal >>= within
  where
    within value
        | minimum' <= value && value <= maximum' = pure value
        | otherwise = fail "out of range"

checkedIntegral :: forall a. (Integral a, Bounded a) => Integer -> Parser a
checkedIntegral number
    | number < toInteger (minBound @a) || number > toInteger (maxBound @a) = fail "Decimal is outside the target type's bounds"
    | otherwise = pure (fromInteger number)

{- | Require the supplied grammar to consume all input; return formatted parse errors as
text.
-}
parseText :: Parser a -> Text -> Either Text a
parseText parser = parse (parser <* eof) "input" >>> first (toText . errorBundlePretty)
