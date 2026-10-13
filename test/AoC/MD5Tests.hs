module AoC.MD5Tests (tests) where

import AoC.MD5 (hexNibbles, md5Hex, md5HexRepeated)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "md5 hex"
        [ testCase "standard empty digest" (md5Hex "" @?= "d41d8cd98f00b204e9800998ecf8427e")
        , testCase "standard single-byte digest" (md5Hex "a" @?= "0cc175b9c0f1b6a831c399e269772661")
        , testCase "standard three-byte digest" (md5Hex "abc" @?= "900150983cd24fb0d6963f7d28e17f72")
        , testCase "standard message digest" (md5Hex "message digest" @?= "f96b697d7cb7938d525a2f31aaf161d0")
        , testCase "nibbles preserve byte order" (hexNibbles "Az" @?= [4, 1, 7, 10])
        , testCase "empty nibbles" (hexNibbles "" @?= [])
        , testCase "nonpositive rounds preserve bytes" $ do
            md5HexRepeated 0 "unchanged" @?= "unchanged"
            md5HexRepeated (-1) "unchanged" @?= "unchanged"
        , testCase "repeated rounds agree with the library on binary messages" $
            for_ [(rounds, bytes) | rounds <- [1, 2, 3, 17, 2017], bytes <- messages] compareRounds
        ]
  where
    messages :: [ByteString]
    messages = ["", "a", "abc", encodeUtf8 (toText (map chr [0 .. 255]))] <> [encodeUtf8 (toText (replicate size 'x')) | size <- [31, 32, 55, 56, 63, 64, 65, 127, 128]]
    compareRounds (rounds, bytes) = md5HexRepeated rounds bytes @?= foldl' (\digest _ -> md5Hex digest) bytes [1 .. rounds]
