-- | Every solution of every year, keyed by puzzle and variant.
module AoC.Registry (Key (..), Variant (..), solvers) where

import AoC.Registry.Types (Key (Key, day, variant, year), Variant (Exploration, Primary))
import AoC.Solution (Solver)
import AoC.Y2016.Registry qualified as Y2016
import Data.Map.Strict qualified as Map

-- ops:root-imports

{- | Registered solvers indexed by year, day and variant; absent keys have no
implementation.
-}
solvers :: Map Key Solver
solvers =
    Map.unions
        [ Map.mapKeys (uncurry (Key 2016)) Y2016.solvers
        -- ops:root-entries
        ]
