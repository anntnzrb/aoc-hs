{- |
approach: Fill both disks by reversed, complemented growth; reduce equal pairs to bits.
cost: O(n + D) time and memory for seed length n and largest disk D; two seed passes.
trade: Strict bytes keeps the literal transformation compact without mutable storage.
-}
module AoC.Y2016.Day16.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day16.Input (Input (Input), parseInput)
import Data.ByteString qualified as Bytes

solver :: Solver
solver text = do
    Input bits <- parseInput text
    let seed = bits & toList & map encode & Bytes.pack
    pure (Answers (decodeUtf8 (diskChecksum 272 seed)) (Just (decodeUtf8 (diskChecksum 35651584 seed))))

encode :: Bool -> Word8
encode False = 48
encode True = 49

diskChecksum :: Int -> ByteString -> ByteString
diskChecksum size seed =
    iterate grow seed
        & find ((>= size) . Bytes.length)
        & fromMaybe seed
        & Bytes.take size
        & checksum

grow :: ByteString -> ByteString
grow bits = bits <> "0" <> (bits & Bytes.reverse & Bytes.map complement)
  where
    complement :: Word8 -> Word8
    complement 48 = 49
    complement _ = 48

reduce :: ByteString -> ByteString
reduce = Bytes.unfoldr pair
  where
    pair :: ByteString -> Maybe (Word8, ByteString)
    pair bits = do
        (a, rest) <- Bytes.uncons bits
        (b, remaining) <- Bytes.uncons rest
        pure (encode (a == b), remaining)

checksum :: ByteString -> ByteString
checksum bits = iterate reduce bits & find (odd . Bytes.length) & fromMaybe bits
