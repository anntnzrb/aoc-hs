{- |
approach: Share a lazy stream of hash summaries; a candidate's first triplet
must match a quintuple in the following thousand summaries. Take 64 keys in
index order, using one MD5 round or 2017 successive lowercase-hex rounds.
cost: O(N * (L + log N + R + W)) time, O(W + L + log N) live memory for salt
length L, rounds R and lookahead W = 1000; one input pass, two index traversals.
trade: Cached lazy lookahead hashes each index once per part and keeps the
matching rule direct, at the cost of scanning a bounded window per triplet.
-}
module AoC.Y2016.Day14.Primary (solver) where

import AoC.MD5 (md5Hex)
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day14.Input (Input (Input), parseInput)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

solver :: Solver
solver text = do
    Input salt <- parseInput text
    pure (Answers (show (keyIndex 1 (encodeUtf8 salt))) (Just (show (keyIndex 2017 (encodeUtf8 salt)))))

keyIndex :: Int -> ByteString -> Natural
keyIndex rounds salt =
    [index | (index, True) <- zip [0 ..] matches]
        & take 64
        & viaNonEmpty last
        & fromMaybe 0
  where
    summaries = [hashSummary rounds (salt <> encodeUtf8 (show index :: Text)) | index <- [0 :: Natural ..]]
    matches = zipWith isKey summaries (drop 1 (tails summaries))

hashSummary :: Int -> ByteString -> (Maybe Char, [Char])
hashSummary rounds bytes = (triplet, quintuples)
  where
    runs = foldl' (\digest _ -> md5Hex digest) bytes [1 .. rounds] & BSC.group
    triplet = find ((>= 3) . BS.length) runs >>= fmap fst . BSC.uncons
    quintuples = [digit | run <- runs, BS.length run >= 5, Just (digit, _) <- [BSC.uncons run]]

isKey :: (Maybe Char, [Char]) -> [(Maybe Char, [Char])] -> Bool
isKey (triplet, _) future =
    triplet & maybe False (\digit -> any (elem digit . snd) (take 1000 future))
