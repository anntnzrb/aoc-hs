{- |
approach: Fold each instruction line over adjacent keypad buttons, retaining the previous button between lines.
cost: O(n) time and O(n) parsed input memory, O(l) output for l lines; two input passes including parsing.
trade: A fixed coordinate map makes rejected moves explicit and keeps the simulation total.
-}
module AoC.Y2016.Day02.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day02.Input (Direction (Downward, Leftward, Rightward, Upward), Input (Input), parseInput)
import Data.Map.Strict qualified as Map

type Position = (Int, Int)

type Button = (Position, Char)

solver :: Solver
solver text = do
    Input rows <- parseInput text
    pure (Answers (code square ((1, 1), '5') rows) (Just "0"))

square :: Map Position Char
square = Map.fromList [((row, column), label) | (row, labels) <- zip [0 ..] ["123", "456", "789"], (column, label) <- zip [0 ..] labels]

code :: Map Position Char -> Button -> NonEmpty (NonEmpty Direction) -> Text
code keypad start rows = rows & mapAccumL press start & snd & toList & toText
  where
    press :: Button -> NonEmpty Direction -> (Button, Char)
    press button moves = (next, snd next)
      where
        next = foldl' (move keypad) button moves

move :: Map Position Char -> Button -> Direction -> Button
move keypad button@(position, _) direction =
    Map.lookup candidate keypad & maybe button (candidate,)
  where
    candidate = adjacent position direction

adjacent :: Position -> Direction -> Position
adjacent (row, column) = \case
    Upward -> (row - 1, column)
    Downward -> (row + 1, column)
    Leftward -> (row, column - 1)
    Rightward -> (row, column + 1)
