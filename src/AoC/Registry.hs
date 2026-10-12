-- | Every solution of every year, keyed by puzzle and variant.
module AoC.Registry (Key (..), Variant (..), solvers) where

import AoC.Solution (Solver)

-- | Which of a day's solutions.
data Variant = Primary | Exploration Text
    deriving stock (Eq, Ord, Show)

-- | A solution's address.
data Key = Key
    { year :: Int
    , day :: Int
    , variant :: Variant
    }
    deriving stock (Eq, Ord, Show)

-- | The union of the per-year registries.
solvers :: Map Key Solver
solvers = mempty
