-- | Every registered variant in 2016.
module AoC.Y2016.Registry (solvers) where

import AoC.Registry.Types (Variant (Primary))
import AoC.Solution (Solver)
import AoC.Y2016.Day01.Primary qualified as Day01Primary
import Data.Map.Strict qualified as Map

import AoC.Y2016.Day02.Primary qualified as Day02Primary

import AoC.Y2016.Day03.Primary qualified as Day03Primary

import AoC.Y2016.Day04.Primary qualified as Day04Primary

import AoC.Y2016.Day05.Primary qualified as Day05Primary

import AoC.Y2016.Day06.Primary qualified as Day06Primary

import AoC.Y2016.Day07.Primary qualified as Day07Primary

import AoC.Y2016.Day08.Primary qualified as Day08Primary

import AoC.Y2016.Day09.Primary qualified as Day09Primary

import AoC.Y2016.Day10.Primary qualified as Day10Primary

import AoC.Y2016.Day11.Primary qualified as Day11Primary

import AoC.Y2016.Day12.Primary qualified as Day12Primary

import AoC.Y2016.Day13.Primary qualified as Day13Primary

import AoC.Y2016.Day14.Primary qualified as Day14Primary

import AoC.Y2016.Day15.Primary qualified as Day15Primary

import AoC.Y2016.Day16.Primary qualified as Day16Primary

import AoC.Y2016.Day17.Primary qualified as Day17Primary

import AoC.Y2016.Day18.Primary qualified as Day18Primary

import AoC.Y2016.Day19.Primary qualified as Day19Primary

import AoC.Y2016.Day20.Primary qualified as Day20Primary

import AoC.Y2016.Day21.Primary qualified as Day21Primary

import AoC.Y2016.Day22.Primary qualified as Day22Primary

import AoC.Y2016.Day23.Primary qualified as Day23Primary

import AoC.Y2016.Day24.Primary qualified as Day24Primary

-- ops:year-imports

solvers :: Map (Int, Variant) Solver
solvers =
    Map.fromList
        [ ((1, Primary), Day01Primary.solver)
        , ((2, Primary), Day02Primary.solver)
        , ((3, Primary), Day03Primary.solver)
        , ((4, Primary), Day04Primary.solver)
        , ((5, Primary), Day05Primary.solver)
        , ((6, Primary), Day06Primary.solver)
        , ((7, Primary), Day07Primary.solver)
        , ((8, Primary), Day08Primary.solver)
        , ((9, Primary), Day09Primary.solver)
        , ((10, Primary), Day10Primary.solver)
        , ((11, Primary), Day11Primary.solver)
        , ((12, Primary), Day12Primary.solver)
        , ((13, Primary), Day13Primary.solver)
        , ((14, Primary), Day14Primary.solver)
        , ((15, Primary), Day15Primary.solver)
        , ((16, Primary), Day16Primary.solver)
        , ((17, Primary), Day17Primary.solver)
        , ((18, Primary), Day18Primary.solver)
        , ((19, Primary), Day19Primary.solver)
        , ((20, Primary), Day20Primary.solver)
        , ((21, Primary), Day21Primary.solver)
        , ((22, Primary), Day22Primary.solver)
        , ((23, Primary), Day23Primary.solver)
        , ((24, Primary), Day24Primary.solver)
        -- ops:year-entries
        ]
