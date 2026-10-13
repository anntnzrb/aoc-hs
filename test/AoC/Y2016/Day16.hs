module AoC.Y2016.Day16 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day16.Primary (solver)
import Data.ByteString qualified as Bytes
import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ops:property-imports

tests :: TestTree
tests =
    testGroup
        "2016/16"
        [ testProperty "2016/16 literal full disks" disks
        -- ops:property-entries
        ]

-- Strict bytes make the literal 35-million-tile disk feasible without a parity oracle.
disks :: Property
disks =
    ( do
        seed <- forAll (Gen.list (Range.constant 1 24) Gen.bool)
        let bytes = Bytes.pack (map (bool 48 49) seed)
        fmap render (solver (decodeUtf8 bytes)) === Right (unlines [checksum 272 bytes, checksum 35651584 bytes])
    )
        & property
        & withTests 2

checksum :: Int -> ByteString -> Text
checksum size seed = fill seed & Bytes.take size & reduce & decodeUtf8
  where
    fill bytes
        | Bytes.length bytes >= size = bytes
        | otherwise = fill (bytes <> one 48 <> Bytes.map (\byte -> if byte == 48 then 49 else 48) (Bytes.reverse bytes))
    reduce bytes
        | odd (Bytes.length bytes) = bytes
        | otherwise = reduce (Bytes.unfoldr pairs bytes)
    pairs :: ByteString -> Maybe (Word8, ByteString)
    pairs bytes = do
        (a, rest) <- Bytes.uncons bytes
        (b, remaining) <- Bytes.uncons rest
        pure (bool 48 49 (a == b), remaining)
