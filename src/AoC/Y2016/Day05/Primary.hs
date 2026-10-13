{- |
approach: Search ascending indices lazily for MD5 digests with twenty leading
zero bits. Accumulate the first eight sixth digits and the first seventh digit
for each valid position until both passwords are complete.
cost: O(N * (L + log N)) time through the last required index, O(L + log N)
live memory for identifier length L; one input pass and one decimal traversal.
trade: A lazy stream expresses the ordered search without storing rejected
hashes; a decimal byte counter avoids text conversion for every rejected hash.
Raw prefix comparison avoids hex encoding those hashes. A bounded map
keeps the first hit for each position and emits characters in position order.
-}
module AoC.Y2016.Day05.Primary (solver) where

import AoC.Solution (Answers, Solver, answers)
import AoC.Y2016.Day05.Digest (increment, nextDigits)
import AoC.Y2016.Day05.Input (Input (Input), parseInput)
import Data.Char (isOctDigit)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

data Passwords = Passwords
    { sequential :: Text
    , positional :: Map Char Char
    }

{- | Return the sequential and position-based eight-character door passwords using
qualifying MD5 hashes. Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input door <- parseInput text
    let hits = [digits | index <- iterate increment "0", Just digits <- [nextDigits (encodeUtf8 door) index]]
    hits
        & scanl' addHit (Passwords "" Map.empty)
        & find complete
        & renderPasswords

addHit :: Passwords -> (Char, Char) -> Passwords
addHit passwords (position, value) =
    Passwords (T.take 8 (passwords.sequential <> one position)) filled
  where
    filled
        | isOctDigit position = Map.insertWith (\_ old -> old) position value passwords.positional
        | otherwise = passwords.positional

complete :: Passwords -> Bool
complete =
    (.positional)
        >>> Map.size
        >>> (== 8)

renderPasswords :: Maybe Passwords -> Either Text Answers
renderPasswords = \case
    Just passwords -> answers passwords.sequential (Just (toText (Map.elems passwords.positional)))
    Nothing -> answers "0" (Just "0")
