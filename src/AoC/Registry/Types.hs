{- | Addresses ordered by year, day (1..25), then variant.
Exploration labels are lowercase module names. Constructors do not
validate; the CLI checks year/day at its boundary.
-}
module AoC.Registry.Types (Key (..), Variant (..)) where

-- | Primary implementation or a named exploration; ordering puts the primary first.
data Variant
    = -- | The idiomatic implementation used as the comparison oracle.
      Primary
    | -- | An alternative implementation labelled by lowercase module name.
      Exploration Text
    deriving stock (Eq, Ord, Show)

{- | A solver address ordered by year, day and variant; constructors do not validate
ranges.
-}
data Key
    = -- | Year, day and implementation in dispatch order.
      Key
      { year :: Int
    -- ^ Puzzle year; the CLI requires at least 2015.
      , day :: Int
    -- ^ Puzzle day; the CLI requires 1 through 25.
      , variant :: Variant
    -- ^ Implementation selected for this puzzle.
      }
    deriving stock (Eq, Ord, Show)
