{- |
approach: Detect ABBA for TLS and intersect outside ABA pairs with reversed inside pairs for SSL.
cost: O(n log k) time, O(n) parsed storage and O(k) sets; k <= 650, one parse and one address pass.
trade: Keep bracket boundaries explicit and use bounded sets to match overlapping SSL patterns.
-}
module AoC.Y2016.Day07.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day07.Input (Address (hypernets, supernets), parseInput)
import Data.List (zip4)
import Data.Set qualified as Set

solver :: Solver
solver text = do
    addresses <- parseInput text
    let (tls, ssl) = foldMap (countSupports supportsTLS &&& countSupports supportsSSL) addresses
    answers (show (getSum tls)) (Just (show (getSum ssl)))

countSupports :: (Address -> Bool) -> Address -> Sum Int
countSupports predicate address
    | predicate address = Sum 1
    | otherwise = Sum 0

supportsTLS :: Address -> Bool
supportsTLS address = any hasABBA address.supernets && not (any hasABBA address.hypernets)

supportsSSL :: Address -> Bool
supportsSSL address = not (Set.disjoint outside inside)
  where
    outside :: Set (Char, Char)
    outside = foldMap abaPairs address.supernets
    inside :: Set (Char, Char)
    inside = foldMap (abaPairs >>> Set.map (\(a, b) -> (b, a))) address.hypernets

abaPairs :: Text -> Set (Char, Char)
abaPairs text = Set.fromList [(a, b) | (a, b, c) <- zip3 chars (drop 1 chars) (drop 2 chars), a == c, a /= b]
  where
    chars :: [Char]
    chars = toString text

hasABBA :: Text -> Bool
hasABBA text =
    zip4 chars (drop 1 chars) (drop 2 chars) (drop 3 chars)
        & any (\(a, b, c, d) -> a /= b && a == d && b == c)
  where
    chars :: [Char]
    chars = toString text
