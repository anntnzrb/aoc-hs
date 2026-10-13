{- |
approach: Share a lazy stream of hash summaries; a candidate's first triplet
must match a quintuple in the following thousand summaries. Take 64 keys in
index order, without skipping hashes used to confirm earlier candidates.
cost: O(N * (L + log N + W)) time, O(W + L + log N) live memory for salt
length L and lookahead W = 1000; one input pass and one index traversal.
trade: Cached lazy lookahead hashes each index once and keeps the matching
rule direct, at the cost of scanning the bounded window for each triplet.
-}
module AoC.Y2016.Day14.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day14.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC

solver :: Solver
solver text = do
    Input salt <- parseInput text
    pure (Answers (show (keyIndex (encodeUtf8 salt))) (Just "0"))

keyIndex :: ByteString -> Natural
keyIndex salt =
    [index | (index, True) <- zip [0 ..] matches]
        & take 64
        & viaNonEmpty last
        & fromMaybe 0
  where
    summaries = [hashSummary (salt <> encodeUtf8 (show index :: Text)) | index <- [0 :: Natural ..]]
    matches = zipWith isKey summaries (drop 1 (tails summaries))

hashSummary :: ByteString -> (Maybe Char, [Char])
hashSummary bytes = (triplet, quintuples)
  where
    runs = bytes & MD5.hash & Base16.encode & BSC.group
    triplet = find ((>= 3) . BS.length) runs >>= fmap fst . BSC.uncons
    quintuples = [digit | run <- runs, BS.length run >= 5, Just (digit, _) <- [BSC.uncons run]]

isKey :: (Maybe Char, [Char]) -> [(Maybe Char, [Char])] -> Bool
isKey (triplet, _) future =
    triplet & maybe False (\digit -> any (elem digit . snd) (take 1000 future))
