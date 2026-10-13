-- | Discs numbered by arrival second, with positive sizes and valid positions.
module AoC.Y2016.Day15.Input (Disc (number, positions, initial), disc, parseInput) where

import AoC.Parse (Parser, parseText)
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

-- | A rotating disc with positive size and an initial position in its cycle.
data Disc = Disc
    { number :: Integer
    -- ^ Positive arrival delay in seconds after release.
    , positions :: Integer
    -- ^ Positive number of positions in one full rotation.
    , initial :: Integer
    -- ^ Position at time zero, from 0 through positions minus one.
    }

{- | Construct a disc from arrival second, position count and initial position; reject
nonpositive numbers or sizes and out-of-cycle positions.
-}
disc :: Integer -> Integer -> Integer -> Either Text Disc
disc number positions initial
    | number <= 0 = Left "Disc numbers must be positive"
    | positions <= 0 = Left "Disc sizes must be positive"
    | initial < 0 || initial >= positions = Left "Position outside disc"
    | otherwise = Right (Disc number positions initial)

{- | Accept disc descriptions in written order; reject nonpositive numbers or sizes and
positions outside the disc. Errors are returned as text.
-}
parseInput :: Text -> Either Text [Disc]
parseInput = parseText (discParser `sepEndBy1` eol)

discParser :: Parser Disc
discParser = do
    number <- string "Disc #" *> decimal
    positions <- string " has " *> decimal
    initial <- string " positions; at time=0, it is at position " *> decimal <* string "."
    disc number positions initial & either (fail . toString) pure
