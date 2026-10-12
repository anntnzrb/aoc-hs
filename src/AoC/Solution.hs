-- | The interface every solution implements: puzzle input to answers.
module AoC.Solution (Answers (..), Solver, render) where

{- | A day's answers in part order; each answer renders as one output line.
'part2' is 'Nothing' only for a puzzle without computational part 2; an
unfinished part answers @0@.
-}
data Answers = Answers
    { part1 :: Text
    , part2 :: Maybe Text
    }

-- | Puzzle input to its answers, or why the input is malformed.
type Solver = Text -> Either Text Answers

-- | One answer per line, part 1 first.
render :: Answers -> Text
render answers = unlines (answers.part1 : maybeToList answers.part2)
