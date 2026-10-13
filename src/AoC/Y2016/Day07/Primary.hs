{- |
approach: Find four-letter mirror patterns outside brackets and exclude addresses with one inside.
cost: O(n) time and O(n) parsed storage; one parsing pass and one address traversal.
trade: Keep bracket boundaries in the domain model so patterns cannot cross them.
-}
module AoC.Y2016.Day07.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day07.Input (Address (hypernets, supernets), parseInput)
import Data.List (zip4)

solver :: Solver
solver text = do
    addresses <- parseInput text
    pure (Answers (show (getSum (foldMap tlsCount addresses))) (Just "0"))

tlsCount :: Address -> Sum Int
tlsCount address
    | supportsTLS address = Sum 1
    | otherwise = Sum 0

supportsTLS :: Address -> Bool
supportsTLS address = any hasABBA address.supernets && not (any hasABBA address.hypernets)

hasABBA :: Text -> Bool
hasABBA text =
    zip4 chars (drop 1 chars) (drop 2 chars) (drop 3 chars)
        & any (\(a, b, c, d) -> a /= b && a == d && b == c)
  where
    chars :: [Char]
    chars = toString text
