{- | Addresses ordered by year, day (1..25), then variant.
Exploration labels are lowercase module names. Constructors do not
validate; the CLI checks year/day at its boundary.
-}
module AoC.Registry.Types (Key (..), Variant (..)) where

data Variant = Primary | Exploration Text
    deriving stock (Eq, Ord, Show)
data Key = Key {year :: Int, day :: Int, variant :: Variant}
    deriving stock (Eq, Ord, Show)
