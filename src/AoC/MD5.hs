{- | MD5 rendered as 32 lowercase ASCII hexadecimal bytes, in digest order.
Input is hashed as bytes exactly as supplied, without text encoding or
normalisation. Callers encode text and compose salts, indices or paths.
Repeated calls hash the previous hexadecimal bytes, not the raw digest.
-}
module AoC.MD5 (md5Hex) where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Base16 qualified as Base16

md5Hex :: ByteString -> ByteString
md5Hex = MD5.hash >>> Base16.encode
