module AoC.Y2016.Day05 (tests) where

import AoC.MD5 (md5Hex)
import AoC.Y2016.Day05.Digest (increment, nextDigits)
import Data.ByteString qualified as BS
import Hedgehog (Gen, Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ops:property-imports

tests :: TestTree
tests =
    testGroup
        "2016/05"
        [ testProperty "2016/05 decimal increment" decimalIncrement
        , testProperty "2016/05 decimal carries" decimalCarries
        , testProperty "2016/05 literal hexadecimal digests" hexadecimalDigests
        , testProperty "2016/05 statement hits" statementHits
        -- ops:property-entries
        ]

decimalIncrement :: Property
decimalIncrement =
    ( do
        index <- forAll (Gen.integral (Range.constant 0 (10 ^ (30 :: Int))) :: Gen Natural)
        increment (show index) === show (index + 1)
    )
        & property
        & withTests 300

decimalCarries :: Property
decimalCarries =
    for_
        (0 : [10 ^ digits - 1 | digits <- [1 .. 30 :: Int]])
        (\index -> increment (show (index :: Natural)) === show (index + 1))
        & property
        & withTests 1

hexadecimalDigests :: Property
hexadecimalDigests =
    ( do
        door <- forAll (Gen.text (Range.constant 1 40) (Gen.filter (\c -> c /= '\n' && c /= '\r') Gen.unicode))
        index <- forAll (Gen.integral (Range.constant 0 (10 ^ (30 :: Int))) :: Gen Natural)
        let bytes = show index
        nextDigits (encodeUtf8 door) bytes === literalDigits (encodeUtf8 door) bytes
    )
        & property
        & withTests 300

statementHits :: Property
statementHits =
    ( do
        for_ [(3231929, ('1', '5')), (5017308, ('8', 'f')), (5357525, ('4', 'e'))] $ \(index, expected) -> do
            let bytes = show (index :: Natural)
            literalDigits "abc" bytes === Just expected
            nextDigits "abc" bytes === Just expected
        nextDigits "abc" "5278568" === literalDigits "abc" "5278568"
        fmap fst (nextDigits "abc" "5278568") === Just 'f'
    )
        & property
        & withTests 1

literalDigits :: ByteString -> ByteString -> Maybe (Char, Char)
literalDigits door index
    | "00000" `BS.isPrefixOf` digest = do
        (sixth, rest) <- BS.uncons (BS.drop 5 digest)
        (seventh, _) <- BS.uncons rest
        pure (chr (fromIntegral sixth), chr (fromIntegral seventh))
    | otherwise = Nothing
  where
    digest = md5Hex (door <> index)
