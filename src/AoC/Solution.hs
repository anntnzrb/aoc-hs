{- | The interface every solution implements: puzzle input to answers.
Answers are non-empty text without CR or LF, constructed only by 'answers'.
Rendering preserves their text and emits one terminated line per part, in order.
Part 2 is absent only without a computational part 2; unfinished parts answer @0@.
-}
module AoC.Solution (Answers, Solver, answers, part1, render) where

import Data.Text qualified as Text

{- | Validated nonempty single-line answers in part order; an absent second part means
no computational part 2.
-}
data Answers = Answers Text (Maybe Text)

-- | Puzzle input to its answers, or why the input or answers are invalid.
type Solver = Text -> Either Text Answers

-- | Reject empty answers and CR or LF in either part; preserve all other text.
answers :: Text -> Maybe Text -> Either Text Answers
answers firstAnswer secondAnswer = do
    traverse_ check (firstAnswer : maybeToList secondAnswer)
    pure (Answers firstAnswer secondAnswer)
  where
    check :: Text -> Either Text ()
    check value
        | value == "" = Left "empty answer"
        | Text.any (`elem` ['\r', '\n']) value = Left "answer contains a line break"
        | otherwise = Right ()

-- | Read the first answer without exposing a record field that could bypass validation.
part1 :: Answers -> Text
part1 (Answers firstAnswer _) = firstAnswer

-- | One terminated line per answer, part 1 first.
render :: Answers -> Text
render (Answers firstAnswer secondAnswer) = unlines (firstAnswer : maybeToList secondAnswer)
