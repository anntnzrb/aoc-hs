{- |
approach: Search ascending indices lazily; take the sixth hex digit of the
first eight MD5 digests whose first twenty bits are zero.
cost: O(N * (L + log N)) time through the last required index, O(L + log N)
live memory for identifier length L; one input pass and one index traversal.
trade: A lazy stream expresses the ordered search without storing rejected
hashes; inspecting raw prefix bytes avoids hex encoding those hashes.
-}
module AoC.Y2016.Day05.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day05.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC

solver :: Solver
solver text = do
    Input door <- parseInput text
    pure (Answers (password (encodeUtf8 door)) (Just "0"))

password :: ByteString -> Text
password door = [digit | index <- [0 :: Natural ..], Just digit <- [nextDigit door index]] & take 8 & toText

nextDigit :: ByteString -> Natural -> Maybe Char
nextDigit door index
    | BS.take 3 digest < BS.pack [0, 0, 16] = digest & Base16.encode & BSC.unpack & drop 5 & viaNonEmpty head
    | otherwise = Nothing
  where
    digest = MD5.hash (door <> encodeUtf8 (show index :: Text))
