module AoC.MD5Tests (tests) where

import AoC.MD5 (md5Hex)
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
        ]
