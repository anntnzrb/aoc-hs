-- | Offline operation contracts; never reads the account configuration.
module OpsTests (tests) where

import Data.Text qualified as T
import Ops.Benchmark (Result (Result), row)
import Ops.Command (moduleName, parseNumber)
import Ops.Haddock (checkHaddock)
import Ops.Logic (Verdict (AlreadyComplete, Cooldown, RightAnswer, UnknownResponse, WrongAnswer), articleText, classify, docViolations, refuseExisting, statement, validAnswer)
import Ops.Measurement (Sample (Sample), Stats (Stats), canonical, cpuList, cpuStats, csvHeader, csvRow, parsePerf, parsePerfCpu, parseUsage, perfUnavailable, runCount, stats)
import Ops.Scaffold (preflightDirectories, scaffold)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests = testGroup "operations" [protocolTests, documentationTests, haddockTests, scaffoldTests, registrationTests, measurementTests, cpuMeasurementTests]

protocolTests :: TestTree
protocolTests =
    testGroup
        "protocol"
        [ testCase "numeric arguments reject overflow before conversion" $ do
            map (isLeft . parseNumber "YEAR" (2015, 9999)) ["18446744073709553631", "-18446744073709549601"] @?= [True, True]
            isLeft (parseNumber "DAY" (1, 25) "18446744073709551617") @?= True
            isLeft (parseNumber "PART" (1, 2) "18446744073709551617") @?= True
            traverse (parseNumber "YEAR" (2015, 9999)) ["2015", "9999"] @?= Right [2015, 9999]
            map (isLeft . parseNumber "PART" (1, 2)) ["0", "3", "oops"] @?= replicate 3 True
        , testCase "answer tokens" $ do
            map validAnswer ["123", "abc-Z,9", "0"] @?= [True, True, True]
            map validAnswer ["", "bad answer", "a\nb", "é", "a&b"] @?= replicate 5 False
        , testCase "submission classifications" $
            map classify ["That's the right answer", "That's not the right answer", "You gave an answer too recently. You have 1m 2s left to wait", "You don't seem to be solving the right level", "maintenance"] @?= [RightAnswer, WrongAnswer, Cooldown, AlreadyComplete, UnknownResponse]
        , testCase "article response" $
            articleText "<nav>secret</nav><article><p>You have 9s left to wait</p></article>" @?= "You have 9s left to wait"
        , testCase "statement isolation" $
            statement "<nav>outside</nav><article class='day-desc'><h2>Title</h2><p>Your puzzle answer was <code>private</code>.</p><pre><code>a\nb</code></pre></article><article>outside too</article>" @?= Right "Title\na\nb\n"
        , testCase "missing statement" $ statement "<article>login</article>" @?= Left "response contains no puzzle articles"
        ]

documentationTests :: TestTree
documentationTests =
    testGroup
        "documentation"
        [ testCase "header accepted" $ docViolations "src/AoC/Y2015/Day01/Primary.hs" "{- |\napproach: idea\ncost: one pass\ntrade: clear\n-}\nmodule A where" @?= []
        , testCase "header rejected" $ null (docViolations "src/AoC/Y2015/Day01/Primary.hs" "module A where") @?= False
        , testCase "String signature rejected" $ null (docViolations "src/AoC/Solution.hs" "f :: String -> Text") @?= False
        , testCase "multiline signature rejected" $ null (docViolations "src/AoC/Solution.hs" "f ::\n    String -> Text") @?= False
        , testCase "comments and literals ignored" $ docViolations "src/AoC/Solution.hs" "-- f :: String\nf :: Text\nf = \"{-# ANN String #-}\"\n" @?= []
        , testCase "constructor after signature ignored" $ docViolations "src/AoC/Solution.hs" "f :: Text\nf String = x" @?= []
        , testCase "annotation rejected" $ null (docViolations "app/A.hs" "{-# ANN f True #-}") @?= False
        ]

scaffoldTests :: TestTree
scaffoldTests =
    testGroup
        "scaffolding"
        [ testCase "planner refuses existing primary" $ isLeft (scaffold (2015, 1) Nothing [("src/AoC/Y2015/Day01/Primary.hs", "existing")]) @?= True
        , testCase "planner refuses exploration without primary" $ isLeft (scaffold (2015, 1) (Just "Lazy") []) @?= True
        , testCase "overwrite refused" $ isLeft (refuseExisting ["Primary.hs"] ["Primary.hs"]) @?= True
        , testCase "case-insensitive overwrite refused" $
            refuseExisting ["LAZY.HS"] ["Lazy.hs"] @?= Left "refusing to overwrite: Lazy.hs"
        , testCase "reserved names refused in every case" $
            map rejected ["Input", "INPUT", "INput", "Primary", "PRIMARY", "PRimary"] @?= replicate 6 True
        , testCase "valid exploration accepted" $ rejected "Lazy" @?= False
        , testCase "fixture file refuses preflight" $
            preflightDirectories [("src/AoC/Y2015/Day01", True), ("fixtures/2015/01", False)] @?= Left "not a directory: fixtures/2015/01"
        , testCase "directory preflight accepts creatable paths" $
            preflightDirectories [("fixtures/2015/01", True)] @?= Right ()
        , testCase "new path accepted" $ refuseExisting ["Input.hs"] ["Primary.hs"] @?= Right ()
        ]
  where
    rejected name = isLeft (moduleName name)

registrationTests :: TestTree
registrationTests =
    testGroup
        "registration anchors"
        [ testCase "existing year retains valid root anchors" $ isRight (scaffold (2015, 2) Nothing (registrationFiles rootSource)) @?= True
        , testCase "existing year rejects missing root anchors" $
            traverse_ (\anchor -> scaffold (2015, 2) Nothing (registrationFiles (T.replace anchor "" rootSource)) @?= Left ("missing or ambiguous scaffold anchor: " <> anchor)) rootAnchors
        , testCase "existing year rejects duplicate root anchors" $
            traverse_ (\anchor -> scaffold (2015, 2) Nothing (registrationFiles (rootSource <> "\n" <> anchor)) @?= Left ("missing or ambiguous scaffold anchor: " <> anchor)) rootAnchors
        ]
  where
    rootAnchors :: [Text]
    rootAnchors = ["-- ops:root-imports", "    -- ops:root-entries"]
    rootSource :: Text
    rootSource = "import AoC.Y2015.Registry qualified as Y2015\n-- ops:root-imports\nsolvers = Map.unions [Y2015.solvers]\n    -- ops:root-entries\n"
    registrationFiles :: Text -> [(FilePath, Text)]
    registrationFiles root =
        [ ("src/AoC/Registry.hs", root)
        , ("src/AoC/Y2015/Registry.hs", "-- ops:year-imports\n    -- ops:year-entries\n")
        , ("aoc.cabal", "    -- ops:solution-modules\n    -- ops:test-modules\n")
        , ("templates/Solver.hs.template", "solver")
        , ("templates/Input.hs.template", "input")
        ]

measurementTests :: TestTree
measurementTests =
    testGroup
        "measurement"
        [ testCase "perf CSV" $ parsePerf "123,,instructions:u,1,100.00,\n" @?= Right (Just 123)
        , testCase "hybrid perf names with inactive other core type" $ parsePerf "<not counted>,,cpu_atom/instructions/u,0,0.00,,\n1234,,cpu_core/instructions/u,1,100.00,," @?= Right (Just 1234)
        , testCase "unavailable perf counters" $ traverse parsePerf ["<not counted>,,instructions:u,0,0,", "<not supported>,,instructions:u,0,0,"] @?= Right [Nothing, Nothing]
        , testCase "invalid perf refuses" $ map (isLeft . parsePerf) ["", "garbage,,instructions:u", "-1,,instructions:u", "1,,instructions:u\n2,,instructions:u"] @?= replicate 4 True
        , testCase "median odd even and empty" $ map stats [[9, 1, 4], [10, 1, 2, 7], []] @?= [Just (Stats 4 1 9), Just (Stats (9 / 2) 1 10), Nothing]
        , testCase "run count boundary" $ map runCount [999999, 1000000, 1000001] @?= [10, 10, 3]
        , testCase "CSV escaping, absence and integer units" $ csvRow [Left "a,\"b\n", Right Nothing, Right (Just (3 / 2)), Right (Just 1234)] @?= "\"a,\"\"b\n\",,2,1234\n"
        , testCase "CSV schema" $ foldl' (\count _ -> count + 1) (0 :: Int) (T.splitOn "," (T.strip csvHeader)) @?= 19
        , testCase "canonical refusal" $ map (uncurry canonical) [("main\n", ""), ("work/12345678", ""), ("", ""), ("main", " M tracked"), ("main", "M  staged")] @?= [Right (), Left "record requires branch main", Left "record requires branch main", Left "record requires a clean tracked tree", Left "record requires a clean tracked tree"]
        , testCase "GNU time RSS only" $ parseUsage "1234\n" @?= Right 1234
        , testCase "GNU time rejects malformed" $ map (isLeft . parseUsage) ["", "nan", "1 2", "-1"] @?= replicate 4 True
        , testCase "CPU lists" $ cpuList "0-2,4,8-9\n" @?= Right [0, 1, 2, 4, 8, 9]
        , testCase "CPU list refuses malformed" $ map (isLeft . cpuList) ["", "2-1", "-1", "a"] @?= replicate 4 True
        ]

cpuMeasurementTests :: TestTree
cpuMeasurementTests =
    testGroup
        "CPU measurement and perf availability"
        [ testCase "perf CPU seconds" $ parsePerfCpu "  0.000950000 seconds user\n  0.001896000 seconds sys\n" @?= Right (Just 2846)
        , testCase "unresolved perf CPU" $ traverse parsePerfCpu ["", "0.000950000 seconds user", "0.000000000 seconds user\n0.000000000 seconds sys", "0.000000400 seconds user\n0.000000400 seconds sys"] @?= Right (replicate 4 Nothing)
        , testCase "invalid perf CPU" $ map (isLeft . parsePerfCpu) ["nan seconds user", "-1 seconds sys", "1 seconds user\n2 seconds user"] @?= replicate 3 True
        , testCase "one unresolved sample invalidates CPU statistic" $ cpuStats [Sample 4000 (Just 2000) 1234, Sample 4100 Nothing 1234, Sample 4200 (Just 3000) 1234] @?= Nothing
        , testCase "complete CPU samples" $ cpuStats [Sample 4000 (Just 2000) 1234, Sample 4200 (Just 3000) 1234] @?= Just (Stats 2500 2000 3000)
        , testCase "CSV unavailable CPU and work fields" $
            T.splitOn "," (T.strip (row (replicate 7 (Left "host") <> [Right (Just 2015), Right (Just 1)]) (Result "primary" [Sample 4000 Nothing 1234] Nothing Nothing))) @?= replicate 7 "\"host\"" <> ["2015", "1", "\"primary\"", "1", "", "", "", "", "4000", "4000", "", "1234"]
        , testCase "perf permission denial is unavailable" $
            perfUnavailable "Error:\nAccess to performance monitoring and observability operations is limited.\nConsider adjusting /proc/sys/kernel/perf_event_paranoid setting to open\naccess to performance monitoring and observability operations for processes\nwithout CAP_PERFMON, CAP_SYS_PTRACE or CAP_SYS_ADMIN Linux capability.\n" @?= True
        , testCase "perf PMU absence is unavailable, solver failures are not" $
            map perfUnavailable ["Error:\nNo permission to enable instructions:u event.\n", "Cannot find PMU `cpu_core'. Missing kernel support?", "Error: No such device", "solver failed: invalid input", "unexpected perf parse error"] @?= [True, True, True, False, False]
        ]

haddockTests :: TestTree
haddockTests =
    testGroup
        "Haddock"
        [ testCase "complete library" $ checkHaddock ["AoC.A", "AoC.B"] (complete <> " 100% ( 12 / 12) in 'AoC.B'\n") @?= Right [T.strip complete, "100% ( 12 / 12) in 'AoC.B'"]
        , testCase "missing exported item" $ isLeft (checkHaddock ["AoC.A"] " 50% ( 1 / 2) in 'AoC.A'\n  Missing documentation for:\n    item (src/AoC/A.hs:1)\n") @?= True
        , testCase "rounded percent cannot hide missing item" $ isLeft (checkHaddock ["AoC.A"] "100% ( 999 / 1000) in 'AoC.A'") @?= True
        , testCase "missing coverage fails closed" $ isLeft (checkHaddock ["AoC.A"] "Up to date\n") @?= True
        , testCase "duplicate coverage refused" $ isLeft (checkHaddock ["AoC.A"] (complete <> complete)) @?= True
        , testCase "unexpected module refused" $ isLeft (checkHaddock ["AoC.B"] complete) @?= True
        , testCase "malformed and invalid coverage refused" $
            traverse_ (\row' -> isLeft (checkHaddock ["AoC.A"] row') @?= True) ["100% broken", "100% ( 0 / 0) in 'AoC.A'", "100% ( 2 / 1) in 'AoC.A'", "101% ( 1 / 1) in 'AoC.A'", "100% ( x / 1) in 'AoC.A'", "100% ( 1 / 1) in AoC.A"]
        , testCase "markup warning refused" $ isLeft (checkHaddock ["AoC.A"] (complete <> "Warning: documentation could not be parsed\n")) @?= True
        , testCase "external link warning refused" $ isLeft (checkHaddock ["AoC.A"] (complete <> "Warning: AoC.A: could not find link destinations for:\n    Data.Text.Text\n")) @?= True
        , testCase "source-located warning refused" $ isLeft (checkHaddock ["AoC.A"] (complete <> "src/AoC/A.hs:1:1: warning: invalid markup\n")) @?= True
        , testCase "Cabal package-index notice is not documentation" $ checkHaddock ["AoC.A"] ("Warning: The package list for 'hackage.haskell.org' does not exist.\n" <> complete) @?= Right [T.strip complete]
        ]
  where
    complete :: Text
    complete = " 100% ( 2 / 2) in 'AoC.A'\n"
