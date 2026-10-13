-- | The input grammar; replace the placeholder with statement-derived types.
module AoC.Y2016.Day16.Input (Input (Input), parseInput) where

newtype Input = Input Text

parseInput :: Text -> Either Text Input
parseInput = Right . Input
