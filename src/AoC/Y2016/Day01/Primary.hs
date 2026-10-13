{- |
approach: Fold turns and whole-block displacements from north, then sum absolute coordinates.
cost: O(n) time and parsed-input memory, O(1) fold state; two passes (parse and fold).
trade: Direct endpoint simulation skips per-block walking; part 2 remains unfinished.
-}
module AoC.Y2016.Day01.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day01.Input (Input (Input), Instruction (Instruction), Steps (Steps), Turn (TurnLeft, TurnRight), parseInput)

data Heading = North | East | South | West

data Location = Location {heading :: Heading, east :: Integer, north :: Integer}

solver :: Solver
solver text = do
    Input route <- parseInput text
    let answer =
            route
                & foldl' walk (Location North 0 0)
                & distance
                & show
    pure (Answers answer (Just "0"))

walk :: Location -> Instruction -> Location
walk location (Instruction turn (Steps steps)) =
    case rotate turn location.heading of
        North -> Location North location.east (location.north + blocks)
        East -> Location East (location.east + blocks) location.north
        South -> Location South location.east (location.north - blocks)
        West -> Location West (location.east - blocks) location.north
  where
    blocks = toInteger steps

rotate :: Turn -> Heading -> Heading
rotate TurnLeft = \case
    North -> West
    East -> North
    South -> East
    West -> South
rotate TurnRight = \case
    North -> East
    East -> South
    South -> West
    West -> North

distance :: Location -> Integer
distance location = abs location.east + abs location.north
