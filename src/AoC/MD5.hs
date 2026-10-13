{- | MD5 rendered as 32 lowercase ASCII hexadecimal bytes, in digest order.
Input is hashed as bytes exactly as supplied, without text encoding or
normalisation. Callers encode text and compose salts, indices or paths.
'md5HexRepeated' applies that transformation the requested number of times;
nonpositive counts return the input unchanged. Subsequent rounds hash the
previous hexadecimal bytes, never the raw digest. The fixed-size rounds use
MD5's standard one-block padding for a 32-byte hexadecimal message.
-}
module AoC.MD5 (md5Hex, md5HexRepeated, hexNibbles) where

import Crypto.Hash.MD5 qualified as MD5
import Data.Bits (complement, rotateL, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16

md5Hex :: ByteString -> ByteString
md5Hex = MD5.hash >>> Base16.encode

{- | High nibble then low nibble per byte, in byte order; values are 0..15.
Works on any bytes, including empty input; no textual encoding is applied.
-}
hexNibbles :: ByteString -> [Word8]
hexNibbles = BS.unpack >>> concatMap (\byte -> [byte `shiftR` 4, byte .&. 15])

data Digest = Digest Word32 Word32 Word32 Word32

data Message = Message Word32 Word32 Word32 Word32 Word32 Word32 Word32 Word32

md5HexRepeated :: Int -> ByteString -> ByteString
md5HexRepeated rounds bytes
    | rounds <= 0 = bytes
    | otherwise = case BS.unpack (MD5.hash bytes) of
        [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] ->
            Digest (word a (b, c, d)) (word e (f, g, h)) (word i (j, k, l)) (word m (n, o, p))
                & repeatDigest (rounds - 1)
                & encodeDigest
        _invalid -> md5Hex bytes

word :: Word8 -> (Word8, Word8, Word8) -> Word32
word a (b, c, d) = fromIntegral a .|. shiftL (fromIntegral b) 8 .|. shiftL (fromIntegral c) 16 .|. shiftL (fromIntegral d) 24

repeatDigest :: Int -> Digest -> Digest
repeatDigest rounds digest@(Digest{})
    | rounds <= 0 = digest
    | otherwise = repeatDigest (rounds - 1) (compress digest)

encodeDigest :: Digest -> ByteString
encodeDigest (Digest a b c d) =
    [fromIntegral (shiftR value offset) | value <- [a, b, c, d], offset <- [0, 8, 16, 24]]
        & BS.pack
        & Base16.encode

-- Spread four bytes into eight lanes, then convert all nibbles together.
hexWord :: Word32 -> Word64
hexWord value = nibbles + 0x3030303030303030 + (((nibbles + 0x0606060606060606) .&. 0x1010101010101010) `shiftR` 4) * 39
  where
    wide = fromIntegral value :: Word64
    pairs = (wide .|. (wide `shiftL` 16)) .&. 0x0000ffff0000ffff
    bytes = (pairs .|. (pairs `shiftL` 8)) .&. 0x00ff00ff00ff00ff
    nibbles = ((bytes .&. 0x000f000f000f000f) `shiftL` 8) .|. ((bytes `shiftR` 4) .&. 0x000f000f000f000f)

compress :: Digest -> Digest
compress (Digest a b c d) =
    Digest 0x67452301 0xefcdab89 0x98badcfe 0x10325476
        & round1 message
        & round2 message
        & round3 message
        & round4 message
        & addInitial
  where
    ha = hexWord a
    hb = hexWord b
    hc = hexWord c
    hd = hexWord d
    message = Message (fromIntegral ha) (fromIntegral (ha `shiftR` 32)) (fromIntegral hb) (fromIntegral (hb `shiftR` 32)) (fromIntegral hc) (fromIntegral (hc `shiftR` 32)) (fromIntegral hd) (fromIntegral (hd `shiftR` 32))

addInitial :: Digest -> Digest
addInitial (Digest a b c d) = Digest (a + 0x67452301) (b + 0xefcdab89) (c + 0x98badcfe) (d + 0x10325476)

round1 :: Message -> Digest -> Digest
round1 (Message m0 m1 m2 m3 m4 m5 m6 m7) (Digest a0 b0 c0 d0) = Digest a4 b4 c4 d4
  where
    a1 = b0 + rotateL (a0 + (d0 `xor` (b0 .&. (c0 `xor` d0))) + m0 + 0xd76aa478) 7
    d1 = a1 + rotateL (d0 + (c0 `xor` (a1 .&. (b0 `xor` c0))) + m1 + 0xe8c7b756) 12
    c1 = d1 + rotateL (c0 + (b0 `xor` (d1 .&. (a1 `xor` b0))) + m2 + 0x242070db) 17
    b1 = c1 + rotateL (b0 + (a1 `xor` (c1 .&. (d1 `xor` a1))) + m3 + 0xc1bdceee) 22
    a2 = b1 + rotateL (a1 + (d1 `xor` (b1 .&. (c1 `xor` d1))) + m4 + 0xf57c0faf) 7
    d2 = a2 + rotateL (d1 + (c1 `xor` (a2 .&. (b1 `xor` c1))) + m5 + 0x4787c62a) 12
    c2 = d2 + rotateL (c1 + (b1 `xor` (d2 .&. (a2 `xor` b1))) + m6 + 0xa8304613) 17
    b2 = c2 + rotateL (b1 + (a2 `xor` (c2 .&. (d2 `xor` a2))) + m7 + 0xfd469501) 22
    a3 = b2 + rotateL (a2 + (d2 `xor` (b2 .&. (c2 `xor` d2))) + 0x80 + 0x698098d8) 7
    d3 = a3 + rotateL (d2 + (c2 `xor` (a3 .&. (b2 `xor` c2))) + 0x8b44f7af) 12
    c3 = d3 + rotateL (c2 + (b2 `xor` (d3 .&. (a3 `xor` b2))) + 0xffff5bb1) 17
    b3 = c3 + rotateL (b2 + (a3 `xor` (c3 .&. (d3 `xor` a3))) + 0x895cd7be) 22
    a4 = b3 + rotateL (a3 + (d3 `xor` (b3 .&. (c3 `xor` d3))) + 0x6b901122) 7
    d4 = a4 + rotateL (d3 + (c3 `xor` (a4 .&. (b3 `xor` c3))) + 0xfd987193) 12
    c4 = d4 + rotateL (c3 + (b3 `xor` (d4 .&. (a4 `xor` b3))) + 256 + 0xa679438e) 17
    b4 = c4 + rotateL (b3 + (a4 `xor` (c4 .&. (d4 `xor` a4))) + 0x49b40821) 22
round2 :: Message -> Digest -> Digest
round2 (Message m0 m1 m2 m3 m4 m5 m6 m7) (Digest a0 b0 c0 d0) = Digest a4 b4 c4 d4
  where
    a1 = b0 + rotateL (a0 + (c0 `xor` (d0 .&. (b0 `xor` c0))) + m1 + 0xf61e2562) 5
    d1 = a1 + rotateL (d0 + (b0 `xor` (c0 .&. (a1 `xor` b0))) + m6 + 0xc040b340) 9
    c1 = d1 + rotateL (c0 + (a1 `xor` (b0 .&. (d1 `xor` a1))) + 0x265e5a51) 14
    b1 = c1 + rotateL (b0 + (d1 `xor` (a1 .&. (c1 `xor` d1))) + m0 + 0xe9b6c7aa) 20
    a2 = b1 + rotateL (a1 + (c1 `xor` (d1 .&. (b1 `xor` c1))) + m5 + 0xd62f105d) 5
    d2 = a2 + rotateL (d1 + (b1 `xor` (c1 .&. (a2 `xor` b1))) + 0x2441453) 9
    c2 = d2 + rotateL (c1 + (a2 `xor` (b1 .&. (d2 `xor` a2))) + 0xd8a1e681) 14
    b2 = c2 + rotateL (b1 + (d2 `xor` (a2 .&. (c2 `xor` d2))) + m4 + 0xe7d3fbc8) 20
    a3 = b2 + rotateL (a2 + (c2 `xor` (d2 .&. (b2 `xor` c2))) + 0x21e1cde6) 5
    d3 = a3 + rotateL (d2 + (b2 `xor` (c2 .&. (a3 `xor` b2))) + 256 + 0xc33707d6) 9
    c3 = d3 + rotateL (c2 + (a3 `xor` (b2 .&. (d3 `xor` a3))) + m3 + 0xf4d50d87) 14
    b3 = c3 + rotateL (b2 + (d3 `xor` (a3 .&. (c3 `xor` d3))) + 0x80 + 0x455a14ed) 20
    a4 = b3 + rotateL (a3 + (c3 `xor` (d3 .&. (b3 `xor` c3))) + 0xa9e3e905) 5
    d4 = a4 + rotateL (d3 + (b3 `xor` (c3 .&. (a4 `xor` b3))) + m2 + 0xfcefa3f8) 9
    c4 = d4 + rotateL (c3 + (a4 `xor` (b3 .&. (d4 `xor` a4))) + m7 + 0x676f02d9) 14
    b4 = c4 + rotateL (b3 + (d4 `xor` (a4 .&. (c4 `xor` d4))) + 0x8d2a4c8a) 20
round3 :: Message -> Digest -> Digest
round3 (Message m0 m1 m2 m3 m4 m5 m6 m7) (Digest a0 b0 c0 d0) = Digest a4 b4 c4 d4
  where
    a1 = b0 + rotateL (a0 + (b0 `xor` c0 `xor` d0) + m5 + 0xfffa3942) 4
    d1 = a1 + rotateL (d0 + (a1 `xor` b0 `xor` c0) + 0x80 + 0x8771f681) 11
    c1 = d1 + rotateL (c0 + (d1 `xor` a1 `xor` b0) + 0x6d9d6122) 16
    b1 = c1 + rotateL (b0 + (c1 `xor` d1 `xor` a1) + 256 + 0xfde5380c) 23
    a2 = b1 + rotateL (a1 + (b1 `xor` c1 `xor` d1) + m1 + 0xa4beea44) 4
    d2 = a2 + rotateL (d1 + (a2 `xor` b1 `xor` c1) + m4 + 0x4bdecfa9) 11
    c2 = d2 + rotateL (c1 + (d2 `xor` a2 `xor` b1) + m7 + 0xf6bb4b60) 16
    b2 = c2 + rotateL (b1 + (c2 `xor` d2 `xor` a2) + 0xbebfbc70) 23
    a3 = b2 + rotateL (a2 + (b2 `xor` c2 `xor` d2) + 0x289b7ec6) 4
    d3 = a3 + rotateL (d2 + (a3 `xor` b2 `xor` c2) + m0 + 0xeaa127fa) 11
    c3 = d3 + rotateL (c2 + (d3 `xor` a3 `xor` b2) + m3 + 0xd4ef3085) 16
    b3 = c3 + rotateL (b2 + (c3 `xor` d3 `xor` a3) + m6 + 0x4881d05) 23
    a4 = b3 + rotateL (a3 + (b3 `xor` c3 `xor` d3) + 0xd9d4d039) 4
    d4 = a4 + rotateL (d3 + (a4 `xor` b3 `xor` c3) + 0xe6db99e5) 11
    c4 = d4 + rotateL (c3 + (d4 `xor` a4 `xor` b3) + 0x1fa27cf8) 16
    b4 = c4 + rotateL (b3 + (c4 `xor` d4 `xor` a4) + m2 + 0xc4ac5665) 23
round4 :: Message -> Digest -> Digest
round4 (Message m0 m1 m2 m3 m4 m5 m6 m7) (Digest a0 b0 c0 d0) = Digest a4 b4 c4 d4
  where
    a1 = b0 + rotateL (a0 + (c0 `xor` (b0 .|. complement d0)) + m0 + 0xf4292244) 6
    d1 = a1 + rotateL (d0 + (b0 `xor` (a1 .|. complement c0)) + m7 + 0x432aff97) 10
    c1 = d1 + rotateL (c0 + (a1 `xor` (d1 .|. complement b0)) + 256 + 0xab9423a7) 15
    b1 = c1 + rotateL (b0 + (d1 `xor` (c1 .|. complement a1)) + m5 + 0xfc93a039) 21
    a2 = b1 + rotateL (a1 + (c1 `xor` (b1 .|. complement d1)) + 0x655b59c3) 6
    d2 = a2 + rotateL (d1 + (b1 `xor` (a2 .|. complement c1)) + m3 + 0x8f0ccc92) 10
    c2 = d2 + rotateL (c1 + (a2 `xor` (d2 .|. complement b1)) + 0xffeff47d) 15
    b2 = c2 + rotateL (b1 + (d2 `xor` (c2 .|. complement a2)) + m1 + 0x85845dd1) 21
    a3 = b2 + rotateL (a2 + (c2 `xor` (b2 .|. complement d2)) + 0x80 + 0x6fa87e4f) 6
    d3 = a3 + rotateL (d2 + (b2 `xor` (a3 .|. complement c2)) + 0xfe2ce6e0) 10
    c3 = d3 + rotateL (c2 + (a3 `xor` (d3 .|. complement b2)) + m6 + 0xa3014314) 15
    b3 = c3 + rotateL (b2 + (d3 `xor` (c3 .|. complement a3)) + 0x4e0811a1) 21
    a4 = b3 + rotateL (a3 + (c3 `xor` (b3 .|. complement d3)) + m4 + 0xf7537e82) 6
    d4 = a4 + rotateL (d3 + (b3 `xor` (a4 .|. complement c3)) + 0xbd3af235) 10
    c4 = d4 + rotateL (c3 + (a4 `xor` (d4 .|. complement b3)) + m2 + 0x2ad7d2bb) 15
    b4 = c4 + rotateL (b3 + (d4 `xor` (c4 .|. complement a4)) + 0xeb86d391) 21

{-# INLINE hexWord #-}
{-# INLINE compress #-}
