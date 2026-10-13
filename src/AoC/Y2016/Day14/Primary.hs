{- |
approach: Share a lazy stream of hash summaries; a candidate's first triplet
must match a quintuple in the following thousand summaries. Take 64 keys in
index order, using one MD5 round or 2017 successive lowercase-hex rounds.
cost: O(N * (L + log N + R + W)) time, O(W + L + log N) live memory for salt
length L, fixed-size rounds R and lookahead W = 1000; one input pass and two
index traversals, with a constant-space round counter.
trade: Cached lazy lookahead hashes each index once per part. A strict fold
over the round range hashes byte buffers directly; triplets
still scan a bounded lookahead window.
-}
module AoC.Y2016.Day14.Primary (solver) where

import AoC.MD5 (md5HexRepeated)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day14.Input (Input (Input), parseInput)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC

-- Lookahead and candidate checks force their own summary fields independently.
data HashSummary = HashSummary
    { triplet :: ~(Maybe Char)
    , quintuples :: ~[Char]
    }

{- | Return the index of the 64th key with plain and 2017-round stretched MD5 hashes.
Input grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input salt <- parseInput text
    answers (show (keyIndex 1 (encodeUtf8 salt))) (Just (show (keyIndex 2017 (encodeUtf8 salt))))

keyIndex :: Int -> ByteString -> Natural
keyIndex rounds salt =
    [index | (index, True) <- zip [0 ..] matches]
        & take 64
        & viaNonEmpty last
        & fromMaybe 0
  where
    summaries = [hashSummary rounds (salt <> encodeUtf8 (show index :: Text)) | index <- [0 :: Natural ..]]
    matches = zipWith isKey summaries (drop 1 (tails summaries))

hashSummary :: Int -> ByteString -> HashSummary
hashSummary rounds bytes = HashSummary triplet quintuples
  where
    runs = md5HexRepeated rounds bytes & BSC.group
    triplet = find ((>= 3) . BS.length) runs >>= fmap fst . BSC.uncons
    quintuples = [digit | run <- runs, BS.length run >= 5, Just (digit, _) <- [BSC.uncons run]]

isKey :: HashSummary -> [HashSummary] -> Bool
isKey summary future =
    summary.triplet & maybe False (\digit -> any (elem digit . (.quintuples)) (take 1000 future))
