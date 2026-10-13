{- | Answer-checked optimized measurements. Timing rounds interleave variants;
five full/empty perf pairs isolate work from dispatcher/runtime startup.
-}
module Ops.Benchmark (benchmark, Result (..), row) where

import AoC.Registry (Key (day, variant, year), Variant (Exploration, Primary), solvers)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Ops.Measurement (Sample (Sample, rss, wall), Stats (maximum, median, minimum), canonical, cpuList, cpuStats, csvHeader, csvRow, parsePerf, perfUnavailable, runCount, stats)
import Ops.Process (abort, cpuTime, peakRss, process, processStatus, timed, utc)
import Ops.Runtime (Runtime, exists, output, readText, writeText)
import Ops.Scaffold (dayText)
import System.Directory (findExecutable)
import System.Exit (ExitCode (ExitSuccess))
import System.Info (os)

{- | No answers are printed or persisted. Record requires clean tracked main
both before measurement and before writing; bench never writes records.
-}
benchmark :: (Runtime :> es) => Bool -> (Int, Int) -> Eff es Int
benchmark recording puzzle = do
    when recording checkCanonical
    _ <- process "cabal" ["build", "exe:aoc"] ""
    (binary, _) <- process "cabal" ["list-bin", "aoc"] ""
    let variants = dayVariants puzzle
    unless (Primary `elem` variants) (abort "no registered primary for this day")
    input <- readText ("inputs/" <> show (fst puzzle) <> "/day" <> toString (dayText (snd puzzle)) <> ".txt")
    when (T.null input) (abort "real input is empty")
    verify (T.strip binary, puzzle) input variants
    host <- hostInfo
    output host.details
    results <- measure (T.strip binary, puzzle) host input
    traverse_ (output . report) results
    traverse_ output host.limitations
    when recording $ do
        checkCanonical
        record puzzle host results
    pure 0

data Host = Host {name :: Text, system :: Text, processor :: Text, compiler :: Text, pin :: [String], perf :: Bool, details :: Text, limitations :: [Text]}
data Result = Result {solution :: Text, samples :: [Sample], work :: Maybe Stats, instructions :: Maybe Stats}

checkCanonical :: (Runtime :> es) => Eff es ()
checkCanonical = do
    (branch, _) <- process "git" ["branch", "--show-current"] ""
    (status, _) <- process "git" ["status", "--porcelain", "--untracked-files=no"] ""
    either abort pure (canonical branch status)

arguments :: (Text, (Int, Int)) -> Variant -> [String]
arguments (binary, (year, day)) variant =
    [toString binary, show year, show day] <> case variant of
        Primary -> []
        Exploration name -> [toString name]

label :: Variant -> Text
label Primary = "primary"
label (Exploration name) = name

verify :: (Runtime :> es) => (Text, (Int, Int)) -> Text -> [Variant] -> Eff es ()
verify target input variants = do
    (reference, _) <- invoke Primary
    traverse_ (compareOutput reference) variants
  where
    invoke variant = case arguments target variant of
        binary : args -> process binary args input
        [] -> abort "missing binary"
    compareOutput reference variant = do
        (answer, _) <- invoke variant
        unless (answer == reference) (abort ("answers disagree: " <> label variant <> "; nothing measured"))

measure :: (Runtime :> es) => (Text, (Int, Int)) -> Host -> Text -> Eff es [Result]
measure target host input = do
    firstRuns <- traverse timing variants
    let counts = map (runCount . (.wall)) firstRuns
    rounds <- traverse (\round' -> traverse (next round') (zip variants counts)) [2 .. 10 :: Int]
    pairs <- if host.perf then traverse (const (traverse workPair variants)) [1 .. 5 :: Int] else pure []
    pure (map (assemble rounds pairs) (zip3 variants [0 ..] firstRuns))
  where
    variants = dayVariants (snd target)
    timing variant = do
        wall <- timed host.pin (arguments target variant) input
        cpu <- if host.perf then cpuTime host.pin (arguments target variant) input else pure Nothing
        rss <- peakRss host.pin (arguments target variant) input
        pure (Sample wall cpu rss)
    next round' (variant, count) = if round' <= count then Just <$> timing variant else pure Nothing
    workPair variant = do
        full <- counter host (arguments target variant) input
        baseline <- counter host (arguments target variant) ""
        pure ((-) <$> full <*> baseline, full)
    assemble rounds pairs (variant, index, firstRun) = Result (label variant) (firstRun : additional) (complete >>= stats) (totals >>= stats)
      where
        additional = mapMaybe (join . viaNonEmpty head . drop index) rounds
        counters = mapMaybe (viaNonEmpty head . drop index) pairs
        complete = traverse fst counters
        totals = traverse snd counters

counter :: (Runtime :> es) => Host -> [String] -> Text -> Eff es (Maybe Integer)
counter host args input = do
    let command = host.pin <> ["perf", "stat", "-x,", "-e", "instructions:u", "--"] <> args
    case command of
        program : rest -> do
            (_, standardError) <- process program rest input
            either abort pure (parsePerf standardError)
        [] -> abort "missing counter command"

-- | Probe a harmless child once, so PMU denial cannot hide a solver failure.
probePerf :: (Runtime :> es) => [String] -> Eff es (Bool, Text, [Text])
probePerf pin
    | os /= "linux" = pure (False, "unavailable on macOS", ["Work, whole-process instructions and CPU time unavailable on macOS; no substitute metric."])
    | otherwise = do
        executable <- liftIO (findExecutable "perf")
        case executable of
            Nothing -> pure (False, "not installed", ["Work, whole-process instructions and CPU time unavailable: perf is not installed."])
            Just _path -> do
                (version, _) <- process "perf" ["--version"] ""
                (available, limits) <- probeCounters pin
                pure (available, T.strip version, limits)

probeCounters :: (Runtime :> es) => [String] -> Eff es (Bool, [Text])
probeCounters pin = case pin <> ["perf", "stat", "-x,", "-e", "instructions:u", "--", "true"] of
    program : args -> do
        (code, _, diagnostic) <- processStatus program args ""
        if code == ExitSuccess
            then do
                count <- either abort pure (parsePerf diagnostic)
                pure (isJust count, ["Work, whole-process instructions and CPU time unavailable: perf probe returned not counted/not supported." | isNothing count])
            else
                if perfUnavailable diagnostic
                    then pure (False, ["Work, whole-process instructions and CPU time unavailable: perf probe failed:\n" <> T.strip diagnostic])
                    else abort ("unexpected perf probe failure:\n" <> diagnostic)
    [] -> abort "missing perf probe command"

optionalFile :: (Runtime :> es) => FilePath -> Eff es Text
optionalFile path = do
    present <- exists path
    if present then T.strip <$> readText path else pure "unavailable"

hostInfo :: (Runtime :> es) => Eff es Host
hostInfo = do
    (name, _) <- process "hostname" [] ""
    (system, _) <- process "uname" ["-srm"] ""
    (compiler, _) <- process "ghc" ["--numeric-version"] ""
    (cabalVersion, _) <- process "cabal" ["--numeric-version"] ""
    tasksetVersion <- if os == "linux" then fst <$> process "taskset" ["--version"] "" else pure "unavailable on macOS"
    processor <- cpuModel
    (pin, coreDetails, pinLimits) <- affinity
    paranoid <- optionalFile "/proc/sys/kernel/perf_event_paranoid"
    (timeVersion, _) <- process "time" ["--version"] ""
    (perf, perfVersion, perfLimits) <- probePerf pin
    let limitations = pinLimits <> perfLimits <> ["Wall: separate runs of the solver under taskset only (no wrapper on macOS); monotonic elapsed microseconds include process spawn and stdin/stdout piping.", "CPU: separate taskset perf stat -- solver runs (no GNU time), interleaved with wall runs and across variants; default user+sys seconds include solver startup, printed to 1 ns, stored at 1 us resolution. If any sample is missing or below 1 us, the entire CPU statistic is unavailable; GNU time's 10 ms CPU fields are not used.", "RSS: separate taskset GNU time -f %M -- solver runs (no taskset on macOS); child peak resident set in KiB, maximum across runs; includes solver runtime startup, no CPU fields used."] <> ["Work: taskset perf stat -x, -e instructions:u -- solver runs, full minus empty, exactly five interleaved pairs; hardware counts may vary; no generated-code mechanism inferred." | perf]
        details = unlines ["Host: " <> T.strip name, "OS/kernel/architecture: " <> T.strip system, "CPU: " <> processor, coreDetails, "perf_event_paranoid: " <> paranoid, "Compiler: ghc " <> T.strip compiler <> " (GHC2024, cabal -O1)", "cabal: " <> T.strip cabalVersion, "taskset: " <> T.strip tasksetVersion, "perf: " <> T.strip perfVersion, "GNU time: " <> fromMaybe "unknown" (viaNonEmpty head (lines timeVersion))]
    pure (Host (T.strip name) (T.strip system) processor ("ghc " <> T.strip compiler) pin perf details limitations)

cpuModel :: (Runtime :> es) => Eff es Text
cpuModel
    | os == "linux" = do
        info <- readText "/proc/cpuinfo"
        pure (fromMaybe "unknown" (viaNonEmpty head [T.strip value | line <- lines info, Just value <- [T.stripPrefix "model name\t:" line]]))
    | otherwise = T.strip . fst <$> process "sysctl" ["-n", "machdep.cpu.brand_string"] ""

affinity :: (Runtime :> es) => Eff es ([String], Text, [Text])
affinity
    | os /= "linux" = pure ([], "Core types/pinned CPU: unavailable", ["CPU affinity unavailable on macOS; migration between core types may affect timings."])
    | otherwise = do
        status <- readText "/proc/self/status"
        allowed <- either abort pure (cpuList (fromMaybe "" (viaNonEmpty head [T.strip value | line <- lines status, Just value <- [T.stripPrefix "Cpus_allowed_list:" line]])))
        cores <- optionalFile "/sys/devices/cpu_core/cpus"
        atoms <- optionalFile "/sys/devices/cpu_atom/cpus"
        let preferred = either (const []) (filter (`elem` allowed)) (cpuList cores)
            selected = viaNonEmpty head (preferred <> allowed)
            limits = ["Hybrid core information unavailable or no permitted P-core; pinned to a fixed permitted CPU." | null preferred]
        cpu <- maybe (abort "no permitted CPU for pinning") pure selected
        pure (["taskset", "-c", show cpu], "Core types: P=" <> cores <> "; E=" <> atoms <> "; pinned CPU=" <> show cpu, limits)

report :: Result -> Text
report result = unlines [result.solution <> ": runs=" <> show (foldl' (\count _ -> count + 1) (0 :: Int) result.samples), "  work instructions " <> range result.work, "  whole-process instructions " <> range result.instructions, "  wall us " <> range (stats (map (.wall) result.samples)), "  cpu us " <> range (cpuStats result.samples), "  rss peak KiB " <> maybe "unavailable" (decimal . (.maximum)) (stats (map (.rss) result.samples))] <> counterLimit <> cpuLimit
  where
    range :: Maybe Stats -> Text
    range Nothing = "unavailable (median/min/max unavailable)"
    range (Just value) = "median=" <> decimal value.median <> " min=" <> decimal value.minimum <> " max=" <> decimal value.maximum
    decimal :: Rational -> Text
    decimal value = if denominator value == 1 then show (numerator value) else show (fromRational value :: Double)
    counterLimit = if isNothing result.work then "  Limitation: work counters unavailable; see host diagnostic.\n" else ""
    cpuLimit = if isNothing (cpuStats result.samples) then "  Limitation: CPU sample missing or below 1 us resolution; see host diagnostic for perf availability.\n" else ""

record :: (Runtime :> es) => (Int, Int) -> Host -> [Result] -> Eff es ()
record puzzle host results = do
    date <- utc
    (commit, _) <- process "git" ["rev-parse", "HEAD"] ""
    let path = "bench/" <> show (fst puzzle) <> ".csv"
        snapshot = "bench/hosts/" <> toString host.name <> ".md"
    present <- exists path
    previous <- if present then readText path else pure csvHeader
    unless (csvHeader `T.isPrefixOf` previous && "\n" `T.isSuffixOf` previous) (abort "existing benchmark CSV has wrong header or unterminated rows")
    snapshotPresent <- exists snapshot
    unless snapshotPresent (writeText snapshot ("# Measurement host\n\n" <> host.details <> "\n" <> unlines (host.limitations <> ["perf returned unavailable counters in this run." | any (isNothing . (.work)) results && host.perf] <> ["CPU time unavailable in this run: at least one sample was missing or below 1 us resolution." | any (isNothing . cpuStats . (.samples)) results])))
    writeText path (previous <> foldMap (row (map Left [date, T.strip commit, host.name, host.system, host.processor, host.compiler, "GHC2024"] <> map (Right . Just . fromIntegral) [fst puzzle, snd puzzle])) results)

row :: [Either Text (Maybe Rational)] -> Result -> Text
row prefix result = csvRow (prefix <> [Left result.solution, Right (Just (fromIntegral (foldl' (\count _ -> count + 1) (0 :: Int) result.samples)))] <> map Right [(.median) <$> result.work, (.minimum) <$> result.work, (.maximum) <$> result.work, (.median) <$> result.instructions, (.minimum) <$> wall, (.median) <$> wall, (.median) <$> cpu, (.maximum) <$> rss])
  where
    wall = stats (map (.wall) result.samples)
    cpu = cpuStats result.samples
    rss = stats (map (.rss) result.samples)

dayVariants :: (Int, Int) -> [Variant]
dayVariants puzzle = [key.variant | key <- Map.keys solvers, (key.year, key.day) == puzzle]
