module Main (main) where

import Control.Exception (try)
import Ops.Command (commandParser, execute)
import Ops.Runtime (runRuntime)
import Options.Applicative (execParser, failureCode, fullDesc, helper, info)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))

main :: IO ()
main = do
    command <- execParser (info (commandParser <**> helper) (fullDesc <> failureCode 4))
    result <- try @SomeException (runRuntime (execute command))
    code <- case result of
        Left _ -> putTextLn "operation failed (filesystem, tool or HTTP error); no credentials logged" $> 4
        Right value -> pure value
    exitWith (bool (ExitFailure code) ExitSuccess (code == 0))
