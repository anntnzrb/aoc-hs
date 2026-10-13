{- | Rows of three positive side lengths, preserving their written order.
Parsing rejects malformed rows and consumes the entire input.
-}
module AoC.Y2016.Day03.Input (Triangle (Triangle), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (eol, hspace, hspace1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

{- | Candidate side lengths in written order; parsing requires positive values and the
solver checks the triangle inequality.
-}
data Triangle
    = -- | Sides in their written order.
      Triangle Natural Natural Natural

{- | Accept whitespace-separated rows of three positive decimals; reject zero sides and
malformed rows. Errors are returned as text.
-}
parseInput :: Text -> Either Text [Triangle]
parseInput = parseText (triangle `sepEndBy1` eol)

triangle :: Parser Triangle
triangle = hspace *> (Triangle <$> side <* hspace1 <*> side <* hspace1 <*> side) <* hspace

side :: Parser Natural
side = do
    value <- Lexer.decimal
    guard (value > 0)
    pure value
