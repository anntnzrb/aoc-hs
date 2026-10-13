{- | MD5 rendered as 32 lowercase ASCII hexadecimal bytes, in digest order.
Input is hashed as bytes exactly as supplied, without text encoding or
normalisation. Callers encode text and compose salts, indices or paths.
'md5HexRepeated' applies that transformation the requested number of times;
nonpositive counts return the input unchanged. Subsequent rounds hash the
previous hexadecimal bytes, never the raw digest.
-}
module AoC.MD5 (md5Hex, md5HexRepeated, hexNibbles) where

import Crypto.Hash.MD5 qualified as MD5
import Data.Bits (shiftR, (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16

{- | Hash bytes exactly as supplied and return 32 lowercase ASCII hexadecimal bytes in
digest order.
-}
md5Hex :: ByteString -> ByteString
md5Hex = MD5.hash >>> Base16.encode

{- | High nibble then low nibble per byte, in byte order; values are 0..15.
Works on any bytes, including empty input; no textual encoding is applied.
-}
hexNibbles :: ByteString -> [Word8]
hexNibbles = BS.unpack >>> concatMap (\byte -> [byte `shiftR` 4, byte .&. 15])

-- | Strictly hash hexadecimal bytes each round; nonpositive counts are identity.
md5HexRepeated :: Int -> ByteString -> ByteString
md5HexRepeated rounds bytes = foldl' (\digest _ -> md5Hex digest) bytes [1 .. rounds]
