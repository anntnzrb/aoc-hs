{- |
approach: Reduce each checksum block to parity, querying mirrored dragon prefixes without materializing the disk.
cost: O(c (n + log D)) time, O(n + c + log D) memory for seed n, disk D and odd checksum length c; one parse and encoding pass, bounded seed prefix traversals.
trade: Prefix parity cancels the reflected halves, replacing disk-sized byte buffers with logarithmic queries; strict bytes retain the seed and output.
-}
module AoC.Y2016.Day16.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day16.Input (Input (Input), parseInput)
import Data.ByteString qualified as Bytes

solver :: Solver
solver text = do
    Input bits <- parseInput text
    let seed =
            bits
                & toList
                & map encode
                & Bytes.pack
    pure (Answers (decodeUtf8 (diskChecksum 272 seed)) (Just (decodeUtf8 (diskChecksum 35651584 seed))))

encode :: Bool -> Word8
encode False = 48
encode True = 49

diskChecksum :: Int -> ByteString -> ByteString
diskChecksum size seed = Bytes.pack [encode (if block == 1 then parity else not parity) | end <- [block, 2 * block .. size], let parity = prefix (end - block) /= prefix end]
  where
    -- Every positive number of pair reductions yields the complement of block parity.
    block =
        iterate (`div` 2) size
            & takeWhile even
            & foldl' (\width _ -> 2 * width) 1
    levels =
        iterate (\n -> 2 * n + 1) (Bytes.length seed)
            & takeWhile (< size)
            & flipfoldl' (:) []
    prefix = prefixParity seed levels

prefixParity :: ByteString -> [Int] -> Int -> Bool
prefixParity seed levels count = seedParity /= adjustment
  where
    (remaining, adjustment) = foldl' descend (count, False) levels
    seedParity = Bytes.take remaining seed & Bytes.foldl' (\parity byte -> parity /= (byte == 49)) False

    descend :: (Int, Bool) -> Int -> (Int, Bool)
    descend prefixState@(!prefixLength, !parity) previous
        | prefixLength <= previous = prefixState
        -- The right prefix is a complemented suffix: its parity cancels the full left half.
        | otherwise = (2 * previous + 1 - prefixLength, parity /= odd (prefixLength - previous - 1))
