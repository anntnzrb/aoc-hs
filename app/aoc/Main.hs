-- | @aoc YEAR DAY [EXPLORATION]@: run one solution as a stdin filter.
module Main (main) where

import AoC.Registry (Key (Key), Variant (Exploration, Primary), solvers)
import AoC.Solution (render)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
    args <- getArgs <&> map toText
    key <- parseKey args & orDie
    input <- ByteString.getContents
    unless (ByteString.null input) $ do
        solver <-
            Map.lookup key solvers
                & maybeToRight "no such solution"
                & orDie
        answers <-
            input
                & decodeUtf8
                & solver
                & orDie
        render answers & putText

parseKey :: [Text] -> Either Text Key
parseKey = \case
    [] -> usage
    [_] -> usage
    [year, day] -> key year day Primary
    [year, day, name] -> key year day (Exploration name)
    _ : _ : _ : _ : _ -> usage
  where
    usage :: Either Text Key
    usage = Left "usage: aoc YEAR DAY [EXPLORATION]"
    key :: Text -> Text -> Variant -> Either Text Key
    key year day variant = Key <$> number year <*> number day <*> pure variant
    number :: Text -> Either Text Int
    number text =
        text
            & toString
            & readMaybe
            & maybeToRight ("not a number: " <> text)

orDie :: Either Text a -> IO a
orDie = either (die . toString) pure
