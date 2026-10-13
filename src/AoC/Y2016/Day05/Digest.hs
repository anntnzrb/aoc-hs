{- | Decimal index stepping and qualifying MD5 digits for the door-password search.
Callers supply the door's UTF-8 bytes and a canonical decimal ASCII index.
Neither operation performs text encoding or input validation.
approach: Increment decimal bytes with right-to-left carry and read qualifying
password digits directly from the MD5 digest's leading bytes and nibbles.
cost: O(D) time and memory per increment for D index digits; O(L + D) hashing
time and memory for L door bytes. One index traversal; no puzzle-input passes.
trade: Avoid per-index text conversion and full hexadecimal encoding while
exposing the two operations for differential checks against literal references.
-}
module AoC.Y2016.Day05.Digest (increment, nextDigits) where

import AoC.MD5 (hexNibbles)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS

{- | Hash the concatenated door and index bytes exactly as supplied. Return the
sixth and seventh lowercase hexadecimal characters, in that order, precisely
when the digest starts with five hexadecimal zeroes; otherwise return 'Nothing'.
Positions outside 0..7 are returned too, for the caller to interpret.
-}
nextDigits :: ByteString -> ByteString -> Maybe (Char, Char)
nextDigits door index = case BS.uncons digest of
    Just (0, rest) -> case BS.uncons rest of
        Just (0, suffix) -> case hexNibbles suffix of
            0 : position : value : _ -> Just (hexDigit position, hexDigit value)
            _other -> Nothing
        _other -> Nothing
    _other -> Nothing
  where
    digest = MD5.hash (door <> index)

hexDigit :: Word8 -> Char
hexDigit digit
    | digit < 10 = chr (fromIntegral digit + ord '0')
    | otherwise = chr (fromIntegral digit + ord 'a' - 10)

{- | Increment a nonempty canonical decimal ASCII natural number, with no leading
zeroes except for "0". Carries may grow the result by one digit; there is no
numeric overflow. Bytes outside this precondition are not validated.
-}
increment :: ByteString -> ByteString
increment bytes = case BS.mapAccumR carry True bytes of
    (True, digits) -> BS.cons 49 digits
    (False, digits) -> digits
  where
    carry :: Bool -> Word8 -> (Bool, Word8)
    carry overflow digit
        | not overflow = (False, digit)
        | digit == 57 = (True, 48)
        | otherwise = (False, digit + 1)
