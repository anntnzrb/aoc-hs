{- |
approach: Search ascending indices lazily for MD5 digests with twenty leading
zero bits. Accumulate the first eight sixth digits and the first seventh digit
for each valid position until both passwords are complete.
cost: O(N * (L + log N)) time through the last required index, O(L + log N)
live memory for identifier length L; one input pass and one index traversal.
trade: A lazy stream expresses the ordered search without storing rejected
hashes; raw prefix comparison avoids hex encoding those hashes. A bounded map
keeps the first hit for each position and emits characters in position order.
-}
module AoC.Y2016.Day05.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day05.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.Char (isOctDigit)
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    Input door <- parseInput text
    let hits = [digits | index <- [0 :: Natural ..], Just digits <- [nextDigits (encodeUtf8 door) index]]
    pure (hits & scanl' addHit ([], Map.empty) & find complete & renderPasswords)

nextDigits :: ByteString -> Natural -> Maybe (Char, Char)
nextDigits door index
    | BS.take 3 digest < BS.pack [0, 0, 16] = do
        (position, rest) <- digest & Base16.encode & BSC.drop 5 & BSC.uncons
        (value, _) <- BSC.uncons rest
        pure (position, value)
    | otherwise = Nothing
  where
    digest = MD5.hash (door <> encodeUtf8 (show index :: Text))

addHit :: ([Char], Map Char Char) -> (Char, Char) -> ([Char], Map Char Char)
addHit (sequential, positional) (position, value) =
    (take 8 (sequential <> [position]), filled)
  where
    filled
        | isOctDigit position = Map.insertWith (\_ old -> old) position value positional
        | otherwise = positional

complete :: ([Char], Map Char Char) -> Bool
complete = snd >>> Map.size >>> (== 8)

renderPasswords :: Maybe ([Char], Map Char Char) -> Answers
renderPasswords = \case
    Just (sequential, positional) -> Answers (toText sequential) (Just (toText (Map.elems positional)))
    Nothing -> Answers "0" (Just "0")
