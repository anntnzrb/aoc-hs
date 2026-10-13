{- | Process interpreter boundaries. Commands use argument vectors and a C
locale, never a shell. Wall, CPU and RSS use separate child processes.
-}
module Ops.Process (process, processStatus, timed, cpuTime, peakRss, abort, utc) where

import Control.Exception (bracket)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Effectful (Eff, (:>))
import GHC.Clock (getMonotonicTimeNSec)
import Ops.Measurement (parsePerfCpu, parseUsage)
import Ops.Runtime (Runtime, output)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose, openTempFile)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)

process :: (Runtime :> es) => FilePath -> [String] -> Text -> Eff es (Text, Text)
process program arguments input = do
    (code, standardOutput, standardError) <- processStatus program arguments input
    if code == ExitSuccess
        then pure (standardOutput, standardError)
        else abort ("tool failed: " <> toText program <> "\n" <> standardError)

-- | Exposes failures for a probe that cannot run the solver; callers classify them.
processStatus :: (Runtime :> es) => FilePath -> [String] -> Text -> Eff es (ExitCode, Text, Text)
processStatus program arguments input = do
    (code, standardOutput, standardError) <- liftIO (run (proc program arguments) input)
    pure (code, toText standardOutput, toText standardError)

abort :: (Runtime :> es) => Text -> Eff es a
abort message = output message >> liftIO (fail "operation aborted")

-- | Only affinity wraps the solver; elapsed time includes spawn and pipe IO.
timed :: (Runtime :> es) => [String] -> [String] -> Text -> Eff es Integer
timed pin arguments input = liftIO $ do
    start <- getMonotonicTimeNSec
    (code, _, diagnostic) <- run (childCommand (pin <> arguments)) input
    end <- getMonotonicTimeNSec
    unless (code == ExitSuccess) (fail ("timed child failed: " <> diagnostic))
    pure (toInteger (end - start) `div` 1000)

-- | perf reports solver user+sys CPU, without a GNU time launcher.
cpuTime :: (Runtime :> es) => [String] -> [String] -> Text -> Eff es (Maybe Integer)
cpuTime pin arguments input = do
    (code, _, diagnostic) <- liftIO (run (childCommand (pin <> ["perf", "stat", "--"] <> arguments)) input)
    unless (code == ExitSuccess) (abort ("CPU child failed: " <> toText diagnostic))
    either abort pure (parsePerfCpu (toText diagnostic))

-- | GNU time reports only this solver child's peak RSS; its CPU is unused.
peakRss :: (Runtime :> es) => [String] -> [String] -> Text -> Eff es Integer
peakRss pin arguments input = do
    usage <-
        liftIO
            ( bracket
                temporary
                removeFile
                ( \path -> do
                    (code, _, standardError) <- run (childCommand (pin <> ["time", "-o", path, "-f", "%M", "--"] <> arguments)) input
                    unless (code == ExitSuccess) (fail ("RSS child failed: " <> standardError))
                    readFileBS path <&> decodeUtf8
                )
            )
    either abort pure (parseUsage usage)
  where
    temporary = do
        directory <- getTemporaryDirectory
        (path, handle) <- openTempFile directory "aoc-time"
        hClose handle
        pure path

utc :: (Runtime :> es) => Eff es Text
utc = liftIO (getCurrentTime <&> toText . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ")

run :: CreateProcess -> Text -> IO (ExitCode, String, String)
run command input = do
    environment <- getEnvironment
    readCreateProcessWithExitCode (command{env = Just (("LC_ALL", "C") : filter ((/= "LC_ALL") . fst) environment)}) (toString input)

childCommand :: [String] -> CreateProcess
childCommand [] = proc "" []
childCommand (program : arguments) = proc program arguments
