{- |
approach: Search ascending indices lazily for MD5 digests with twenty leading
zero bits. Accumulate the first eight sixth digits and the first seventh digit
for each valid position until both passwords are complete.
cost: O(N * (L + log N)) time through the last required index, O(L + log N)
live memory for identifier length L; one input pass and one decimal traversal.
trade: A lazy stream expresses the ordered search without storing rejected
hashes; a decimal byte counter avoids text conversion for every rejected hash.
Raw prefix comparison avoids hex encoding those hashes. A bounded map
keeps the first hit for each position and emits characters in position order.
-}
module AoC.Y2016.Day05.Primary (solver) where

import AoC.MD5 (hexNibbles)
import AoC.Solution (Answers, Solver, answers)
import AoC.Y2016.Day05.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS
import Data.Char (isOctDigit)
import Data.Map.Strict qualified as Map

solver :: Solver
solver text = do
    Input door <- parseInput text
    let hits = [digits | index <- iterate increment "0", Just digits <- [nextDigits (encodeUtf8 door) index]]
    hits
        & scanl' addHit ([], Map.empty)
        & find complete
        & renderPasswords

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

addHit :: ([Char], Map Char Char) -> (Char, Char) -> ([Char], Map Char Char)
addHit (sequential, positional) (position, value) =
    (take 8 (sequential <> [position]), filled)
  where
    filled
        | isOctDigit position = Map.insertWith (\_ old -> old) position value positional
        | otherwise = positional

complete :: ([Char], Map Char Char) -> Bool
complete =
    snd
        >>> Map.size
        >>> (== 8)

renderPasswords :: Maybe ([Char], Map Char Char) -> Either Text Answers
renderPasswords = \case
    Just (sequential, positional) -> answers (toText sequential) (Just (toText (Map.elems positional)))
    Nothing -> answers "0" (Just "0")
