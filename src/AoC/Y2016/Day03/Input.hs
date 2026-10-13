{- | Rows of three positive side lengths, preserving their written order.
Parsing rejects malformed rows and consumes the entire input.
-}
module AoC.Y2016.Day03.Input (Triangle (Triangle), parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (eol, hspace, hspace1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Triangle = Triangle Natural Natural Natural

parseInput :: Text -> Either Text [Triangle]
parseInput = parseText (triangle `sepEndBy1` eol)

triangle :: Parser Triangle
triangle = hspace *> (Triangle <$> side <* hspace1 <*> side <* hspace1 <*> side) <* hspace

side :: Parser Natural
side = do
    value <- Lexer.decimal
    guard (value > 0)
    pure value
