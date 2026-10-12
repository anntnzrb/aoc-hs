-- | Every registered solution against every fixture of its day.
module Main (main) where

import AoC.Registry (Key (day, year), solvers)
import AoC.Solution (Solver, render)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (replaceExtension, takeExtension, (</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = do
    tests <- Map.toList solvers & traverse solutionTests
    testGroup "fixtures" tests & defaultMain

-- | A fixture directory: fixtures/YEAR/DD.
fixtureDir :: Key -> FilePath
fixtureDir key = "fixtures" </> show key.year </> dayDir
  where
    dayDir =
        key.day
            & show
            & Text.justifyRight 2 '0'
            & toString

solutionTests :: (Key, Solver) -> IO TestTree
solutionTests (key, solver) = do
    let dir = fixtureDir key
    exists <- doesDirectoryExist dir
    inputs <-
        exists
            & bool (pure []) (listDirectory dir)
            <&> filter ((== ".in") . takeExtension)
    inputs
        & sort
        & map (fixtureTest solver . (dir </>))
        & testGroup (show key)
        & pure

{- | Compare the first k output lines, k being the expected file's line
count; an empty or unterminated expected file fails.
-}
fixtureTest :: Solver -> FilePath -> TestTree
fixtureTest solver input = testCase input $ do
    expected <- readFileBS (replaceExtension input "out") <&> decodeUtf8
    when (Text.null expected || not ("\n" `Text.isSuffixOf` expected)) $
        assertFailure "expected output is empty or unterminated"
    actual <- readFileBS input <&> (solver . decodeUtf8)
    case actual of
        Left err -> assertFailure (toString err)
        Right answers -> do
            let actualLines = lines (render answers)
                expectedLines = lines expected
            zipWith const actualLines expectedLines @?= expectedLines
