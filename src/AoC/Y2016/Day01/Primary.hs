{- |
approach: Fold endpoints for the final distance; stream intervening blocks to find the first revisit.
cost: n instructions, b blocks checked: O(n + b log(b + 1)) time, O(n + b) memory; three passes.
trade: Endpoint jumps keep part 1 cheap; part 2 stores visits and stops at the first repeat.
-}
module AoC.Y2016.Day01.Primary (solver) where

import AoC.Solution (Solver, answers)
import AoC.Y2016.Day01.Input (Input (Input), Instruction (Instruction), Steps (Steps), Turn (TurnLeft, TurnRight), parseInput)
import Data.Set qualified as Set

data Heading = North | East | South | West

data Location = Location {heading :: Heading, east :: Integer, north :: Integer}

{- | Return Manhattan blocks to the final location and first revisited location; use 0
when no revisit exists. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input route <- parseInput text
    let answer =
            route
                & foldl' walk (Location North 0 0)
                & distance
                & show
        revisit =
            firstRevisit route
                & fromMaybe 0
                & show
    -- A route without a revisit still has a valid part-1 answer.
    answers answer (Just revisit)

firstRevisit :: NonEmpty Instruction -> Maybe Integer
firstRevisit route =
    zipWith segmentPositions locations (drop 1 locations)
        & concat
        & foldlM visit (one (0, 0))
        & leftToMaybe
  where
    locations = scanl walk (Location North 0 0) (toList route)

segmentPositions :: Location -> Location -> [Location]
segmentPositions start end =
    [ Location end.heading (start.east + step * signum dx) (start.north + step * signum dy)
    | step <- [1 .. abs dx + abs dy]
    ]
  where
    dx = end.east - start.east
    dy = end.north - start.north

visit :: Set (Integer, Integer) -> Location -> Either Integer (Set (Integer, Integer))
visit seen location
    | Set.member position seen = Left (distance location)
    | otherwise = Right (Set.insert position seen)
  where
    position = (location.east, location.north)

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
