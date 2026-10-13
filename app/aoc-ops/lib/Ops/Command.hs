-- | CLI operations. Every external action runs through Runtime.
module Ops.Command (Command, commandParser, moduleName, parseNumber, execute) where

import AoC.Parse (boundedSignedDecimalWithin, parseText)
import Data.Text qualified as T
import Effectful (Eff, (:>))
import Ops.Benchmark (benchmark)
import Ops.Haddock (checkHaddock)
import Ops.Logic (Verdict (AlreadyComplete, Cooldown, RightAnswer, UnknownResponse, WrongAnswer), articleText, classify, docViolations, statement, validAnswer)
import Ops.Process (process)
import Ops.Runtime (Runtime, directories, exists, formatFiles, http, isDirectory, makeDirectory, output, readText, writeText)
import Ops.Scaffold (dayText, moduleName, preflightDirectories, scaffold)
import Options.Applicative (Parser, ParserInfo, argument, command, eitherReader, hsubparser, info, long, metavar, str, switch)
import System.FilePath (dropExtension, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))

data Command = Fetch Int Int Bool | Puzzle Int Int | Submit Int Int Int Text | New Int Int (Maybe Text) | Bench Int Int | Record Int Int | Doc | Haddock

commandParser :: Parser Command
commandParser =
    hsubparser
        ( command "fetch" (info' (Fetch <$> year <*> day <*> switch (long "force")))
            <> command "puzzle" (info' (Puzzle <$> year <*> day))
            <> command "submit" (info' (Submit <$> year <*> day <*> number "PART" 1 2 <*> argument str (metavar "ANSWER")))
            <> command "new" (info' (New <$> year <*> day <*> optional (argument (eitherReader moduleName) (metavar "EXPLORATION"))))
            <> command "bench" (info' (Bench <$> year <*> day))
            <> command "record" (info' (Record <$> year <*> day))
            <> command "doc" (info' (pure Doc))
            <> command "haddock" (info' (pure Haddock))
        )
  where
    info' :: Parser a -> ParserInfo a
    info' parser = info parser mempty
    year = number "YEAR" 2015 9999
    day = number "DAY" 1 25

number :: String -> Int -> Int -> Parser Int
number label minimum' maximum' = argument (eitherReader (parseNumber label (minimum', maximum'))) (metavar label)

-- | Parse without machine overflow, then enforce inclusive argument bounds.
parseNumber :: String -> (Int, Int) -> String -> Either String Int
parseNumber label bounds text = parseText (boundedSignedDecimalWithin bounds) (toText text) & first labelled
  where
    labelled err
        | "out of range\n" `T.isSuffixOf` err = label <> " out of range"
        | otherwise = label <> ": " <> toString err

execute :: (Runtime :> es) => Command -> Eff es Int
execute = \case
    Fetch year day forced -> fetch year day forced
    Puzzle year day -> do
        result <- request (year, day) "" Nothing
        case result >>= statement of
            Left message -> failure message
            Right text -> writeText ("puzzles" </> show year </> toString (dayText day) <> ".txt") text $> 0
    Submit year day part answer
        | not (validAnswer answer) -> failure "invalid answer: expected [0-9A-Za-z,-]+"
        | otherwise -> do
            result <- request (year, day) "/answer" (Just [("level", encodeUtf8 (show part :: Text)), ("answer", encodeUtf8 answer)])
            case result of
                Left message -> failure message
                Right html -> let text = articleText html in output text $> verdictCode (classify text)
    New year day name -> createScaffold year day name
    Bench year day -> benchmark False (year, day)
    Record year day -> benchmark True (year, day)
    Doc -> checkDocs
    Haddock -> checkLibraryDocs

failure :: (Runtime :> es) => Text -> Eff es Int
failure message = output message $> 4

request :: (Runtime :> es) => (Int, Int) -> Text -> Maybe [(ByteString, ByteString)] -> Eff es (Either Text Text)
request (year, day) suffix body = do
    (status, text) <- http ("https://adventofcode.com/" <> show year <> "/day/" <> show day <> suffix) body
    pure (bool (Left ("HTTP status " <> show status)) (Right text) (status == 200))

fetch :: (Runtime :> es) => Int -> Int -> Bool -> Eff es Int
fetch year day forced = do
    let path = "inputs" </> show year </> "day" <> toString (dayText day) <> ".txt"
    cached <- exists path
    if cached && not forced
        then output ("cached: " <> toText path) $> 0
        else do
            result <- request (year, day) "/input" Nothing
            case result of
                Left message -> failure message
                Right text -> writeText path text $> 0

walk :: (Runtime :> es) => FilePath -> Eff es [FilePath]
walk path = do
    present <- isDirectory path
    if not present
        then pure []
        else do
            entries <- directories path
            fmap concat (traverse descend entries)
  where
    descend entry
        | takeExtension entry == ".hs" = pure [path </> entry]
        | null (takeExtension entry) = walk (path </> entry)
        | otherwise = pure []

checkDocs :: (Runtime :> es) => Eff es Int
checkDocs = do
    paths <- fmap concat (traverse walk ["app", "src", "test"])
    violations <- fmap concat (traverse check paths)
    traverse_ output violations
    pure (bool 4 0 (null violations))
  where
    check :: (Runtime :> es) => FilePath -> Eff es [Text]
    check path = readText path <&> docViolations path

createScaffold :: (Runtime :> es) => Int -> Int -> Maybe Text -> Eff es Int
createScaffold year day name = do
    paths <- fmap concat (traverse walk ["src", "test"])
    snapshots <- traverse snapshot (paths <> ["aoc.cabal"] <> templatePaths)
    case scaffold (year, day) name snapshots of
        Left message -> failure message
        Right writes -> do
            let fixtures = "fixtures" </> show year </> toString (dayText day)
            ready <- preflight (map fst snapshots) (map fst writes) fixtures
            case ready of
                Left message -> failure message
                Right () -> do
                    makeDirectory fixtures
                    traverse_ (uncurry writeText) writes
                    formatFiles (map fst writes & filter ((== ".hs") . takeExtension))
                    pure 0
  where
    snapshot :: (Runtime :> es) => FilePath -> Eff es (FilePath, Text)
    snapshot path = readText path <&> (path,)
    templatePaths :: [FilePath]
    templatePaths = map (\name' -> "templates/" <> name' <> ".hs.template") ["Input", "Solver", "Year", "Property"]

verdictCode :: Verdict -> Int
verdictCode = \case
    RightAnswer -> 0
    WrongAnswer -> 1
    Cooldown -> 2
    AlreadyComplete -> 2
    UnknownResponse -> 3

preflight :: (Runtime :> es) => [FilePath] -> [FilePath] -> FilePath -> Eff es (Either Text ())
preflight snapshots targets fixtures = do
    parents <- traverse checkDirectory (ordNub (concatMap ancestors (fixtures : map takeDirectory targets)))
    files <- traverse checkTarget targets
    pure $ do
        sequence_ parents
        sequence_ files
  where
    ancestors = iterate takeDirectory >>> takeWhile (/= ".")
    checkDirectory :: (Runtime :> es) => FilePath -> Eff es (Either Text ())
    checkDirectory path = do
        collision <- caseCollision path
        present <- exists path
        directory <- isDirectory path
        pure (collision >> preflightDirectories [(path, not present || directory)])
    checkTarget path = do
        collision <- caseCollision path
        present <- exists path
        directory <- isDirectory path
        pure (collision >> when (directory || (present && path `notElem` snapshots)) (Left ("refusing to overwrite: " <> toText path)))

caseCollision :: (Runtime :> es) => FilePath -> Eff es (Either Text ())
caseCollision path = do
    parent <- isDirectory (takeDirectory path)
    entries <- if parent then directories (takeDirectory path) else pure []
    let name = takeFileName path
        aliases = [entry | entry <- entries, T.toCaseFold (toText entry) == T.toCaseFold (toText name), entry /= name]
    pure (unless (null aliases) (Left ("case-insensitive path collision: " <> toText path)))

checkLibraryDocs :: (Runtime :> es) => Eff es Int
checkLibraryDocs = do
    paths <- walk "src/AoC"
    let modules = paths <&> (dropExtension >>> splitDirectories >>> drop 1 >>> map toText >>> T.intercalate ".")
    (standardOutput, standardError) <- process "cabal" ["haddock", "-v0", "lib:aoc"] ""
    case checkHaddock modules (standardOutput <> "\n" <> standardError) of
        Left message -> output (standardOutput <> standardError) >> failure message
        Right rows -> traverse_ output rows $> 0
