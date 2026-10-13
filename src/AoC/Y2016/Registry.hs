-- | Every registered variant in 2016.
module AoC.Y2016.Registry (solvers) where

import AoC.Registry.Types (Variant (Primary))
import AoC.Solution (Solver)
import AoC.Y2016.Day01.Primary qualified as Day01Primary
import Data.Map.Strict qualified as Map

-- ops:year-imports

solvers :: Map (Int, Variant) Solver
solvers =
    Map.fromList
        [ ((1, Primary), Day01Primary.solver)
        -- ops:year-entries
        ]
