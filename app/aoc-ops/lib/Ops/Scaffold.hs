-- | Plan all scaffold writes before applying them. Existing solutions are refused.
module Ops.Scaffold (scaffold, dayText, dayModule, moduleName, preflightDirectories) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (lookup)
import Data.Text qualified as T
import Ops.Logic (refuseExisting)
import System.FilePath ((</>))

dayText :: Int -> Text
dayText day = T.justifyRight 2 '0' (show day)

dayModule :: Int -> Int -> Text
dayModule year day = "AoC.Y" <> show year <> ".Day" <> dayText day

-- | Require an ASCII CamelCase exploration module name, excluding Input and Primary.
moduleName :: String -> Either String Text
moduleName name = case name of
    first' : rest
        | isAsciiUpper first' && all allowed rest && T.toCaseFold (toText name) `notElem` ["primary", "input"] -> Right (toText name)
        | otherwise -> Left invalid
    [] -> Left invalid
  where
    invalid :: String
    invalid = "exploration must be a CamelCase module name other than Input or Primary"
    allowed c = isAsciiUpper c || isAsciiLower c || isDigit c

-- | Snapshots include templates and shared registration files, never secrets.
scaffold :: (Int, Int) -> Maybe Text -> [(FilePath, Text)] -> Either Text [(FilePath, Text)]
scaffold (year, day) exploration files = do
    refuseExisting (map fst files) targets
    when (isJust exploration && primaryPath `notElem` map fst files) (Left "create the primary before an exploration")
    generated <- traverse fromTemplate definitions
    registered <- registration (year, day) exploration files
    pure (generated <> registered)
  where
    base = "src/AoC/Y" <> show year <> "/Day" <> toString (dayText day)
    primaryPath = base </> "Primary.hs"
    name = fromMaybe "Primary" exploration
    targets = [base </> toString name <> ".hs"] <> [base </> "Input.hs" | isNothing exploration]
    definitions :: [(FilePath, Text)]
    definitions = [(base </> toString name <> ".hs", "Solver")] <> [(base </> "Input.hs", "Input") | isNothing exploration]
    fromTemplate (path, template) = do
        source <- lookup ("templates/" <> toString template <> ".hs.template") files & maybeToRight "missing template"
        pure (path, substitute (year, day) name source)

substitute :: (Int, Int) -> Text -> Text -> Text
substitute (year, day) name source = foldl' replace source [("YEAR", show year), ("DAYMODULE", dayModule year day), ("NAME", name)]
  where
    replace text (key, value) = T.replace ("{{" <> key <> "}}") value text

insertAt :: Text -> Text -> Text -> Either Text Text
insertAt anchor insertion source
    | T.count anchor source /= 1 = Left ("missing or ambiguous scaffold anchor: " <> anchor)
    | T.null insertion = Right source
    | otherwise = Right (T.replace anchor (insertion <> "\n" <> anchor) source)

registration :: (Int, Int) -> Maybe Text -> [(FilePath, Text)] -> Either Text [(FilePath, Text)]
registration (year, day) exploration files = do
    yearSource <- lookup yearPath files & maybe (template "Year") Right
    when (isJust exploration && variant `T.isInfixOf` yearSource && ("(" <> show day <> ", " <> variant) `T.isInfixOf` yearSource) (Left "exploration variant already registered")
    let importsReady = bool yearSource (T.replace "Variant (Primary)" "Variant (Primary, Exploration)" yearSource) (isJust exploration)
    imported <- insertAt "-- ops:year-imports" ("import " <> qualifiedModule <> " qualified as " <> alias) importsReady
    table <- insertAt "    -- ops:year-entries" (prefix yearSource <> "((" <> show day <> ", " <> variant <> "), " <> alias <> ".solver)") imported
    root <- rootRegistry year files
    properties <- maybe (Right []) (propertyRegistration (year, day) files) exploration
    cabal <- cabalRegistration modules (map fst properties) files
    pure ([(yearPath, table), ("src/AoC/Registry.hs", root), ("aoc.cabal", cabal)] <> types <> properties)
  where
    yearPath = "src/AoC/Y" <> show year <> "/Registry.hs"
    name = fromMaybe "Primary" exploration
    qualifiedModule = dayModule year day <> "." <> name
    alias = "Day" <> dayText day <> name
    variant = maybe "Primary" (\n -> "Exploration \"" <> T.toLower n <> "\"") exploration
    modules = [qualifiedModule] <> [dayModule year day <> ".Input" | isNothing exploration] <> ["AoC.Y" <> show year <> ".Registry" | isNothing (lookup yearPath files)] <> ["AoC.Registry.Types" | not (null types)]
    types = [("src/AoC/Registry/Types.hs", registryTypes) | isNothing (lookup "src/AoC/Registry/Types.hs" files)]
    template name' = lookup ("templates/" <> name' <> ".hs.template") files & maybe (Left "missing registration template") (Right . substitute (year, day) name)

prefix :: Text -> Text
prefix source = bool "    , " "      " (not (".solver" `T.isInfixOf` source))

registryTypes :: Text
registryTypes =
    unlines
        [ "-- | Addresses ordered by year, day (1..25), then variant."
        , "-- Exploration labels are lowercase module names. Constructors do not"
        , "-- validate; the CLI checks year/day at its boundary."
        , "module AoC.Registry.Types (Key (..), Variant (..)) where"
        , "data Variant = Primary | Exploration Text"
        , "    deriving stock (Eq, Ord, Show)"
        , "data Key = Key { year :: Int, day :: Int, variant :: Variant }"
        , "    deriving stock (Eq, Ord, Show)"
        ]

rootRegistry :: Int -> [(FilePath, Text)] -> Either Text Text
rootRegistry year files = do
    original <- lookup "src/AoC/Registry.hs" files & maybeToRight "missing root registry"
    let source = bool original rootTemplate ("solvers = mempty" `T.isInfixOf` original)
        yearModule = "AoC.Y" <> show year <> ".Registry"
    traverse_ (\anchor -> insertAt anchor "" source $> ()) ["-- ops:root-imports", "    -- ops:root-entries"]
    if yearModule `T.isInfixOf` source
        then Right source
        else do
            imported <- insertAt "-- ops:root-imports" ("import " <> yearModule <> " qualified as Y" <> show year) source
            insertAt "    -- ops:root-entries" (prefixRoot source <> "Map.mapKeys (uncurry (Key " <> show year <> ")) Y" <> show year <> ".solvers") imported
  where
    prefixRoot :: Text -> Text
    prefixRoot source = bool "    , " "      " (not (".solvers" `T.isInfixOf` source))
    rootTemplate =
        unlines
            [ "-- | Every solution of every year, keyed by puzzle and variant."
            , "module AoC.Registry (Key (..), Variant (..), solvers) where"
            , "import AoC.Registry.Types (Key (Key, year, day, variant), Variant (Primary, Exploration))"
            , "import AoC.Solution (Solver)"
            , "import Data.Map.Strict qualified as Map"
            , "-- ops:root-imports"
            , "solvers :: Map Key Solver"
            , "solvers = Map.unions"
            , "    ["
            , "    -- ops:root-entries"
            , "    ]"
            ]

cabalRegistration :: [Text] -> [FilePath] -> [(FilePath, Text)] -> Either Text Text
cabalRegistration modules propertyPaths files = do
    original <- lookup "aoc.cabal" files & maybeToRight "missing aoc.cabal"
    sources <- insertAt "    -- ops:solution-modules" (unlines (map ("    " <>) modules)) original
    insertAt "    -- ops:test-modules" (unlines (map ("    " <>) testModules)) sources
  where
    testModules = [T.replace "/" "." (T.dropEnd 3 (T.drop 5 (toText path))) | path <- propertyPaths, path /= "test/Main.hs", isNothing (lookup path files)]

propertyRegistration :: (Int, Int) -> [(FilePath, Text)] -> Text -> Either Text [(FilePath, Text)]
propertyRegistration (year, day) files name = do
    source <- lookup path files & maybe (lookup "templates/Property.hs.template" files & maybe (Left "missing property template") (Right . substitute (year, day) name)) Right
    imported <- insertAt "-- ops:property-imports" (propertyImports (dayModule year day) (name, source)) source
    propertyText <- insertAt "    -- ops:property-entries" (propertyPrefix source <> propertyEntry name source) imported
    mainSource <- lookup "test/Main.hs" files & maybeToRight "missing test Main"
    wired <-
        if isJust (lookup path files)
            then Right mainSource
            else do
                mainImported <- insertAt "-- ops:test-imports" ("import " <> dayModule year day <> " qualified as Y" <> show year <> "Day" <> dayText day) mainSource
                insertAt "        -- ops:test-entries" ("        , Y" <> show year <> "Day" <> dayText day <> ".tests") mainImported
    pure [(path, propertyText), ("test/Main.hs", wired)]
  where
    path = "test/AoC/Y" <> show year <> "/Day" <> toString (dayText day) <> ".hs"

-- | Missing directories are creatable; existing non-directories refuse the plan.
preflightDirectories :: [(FilePath, Bool)] -> Either Text ()
preflightDirectories states = case [toText path | (path, False) <- states] of
    [] -> Right ()
    blocked -> Left ("not a directory: " <> T.intercalate ", " blocked)

-- Existing test modules need only anchors; generated imports do not depend on
-- their aliases. The empty generator is a scaffold stub to replace with grammar.
propertyImports :: Text -> (Text, Text) -> Text
propertyImports day (name, source) =
    unlines (["import " <> day <> "." <> name <> " qualified as OpsCandidate" <> name] <> filter (not . (`T.isInfixOf` source)) support)
  where
    support =
        if "genInput ::" `T.isInfixOf` source
            then []
            else
                [ "import " <> day <> ".Primary qualified as OpsPrimary"
                , "import AoC.Solution qualified as OpsAnswers"
                , "import Hedgehog qualified as OpsProperty"
                , "import Test.Tasty.Hedgehog qualified as OpsTests"
                ]

propertyEntry :: Text -> Text -> Text
propertyEntry name source
    | "genInput ::" `T.isInfixOf` source = "testProperty \"" <> name <> " agrees with primary\" (property (do { input <- forAll genInput; fmap render (OpsCandidate" <> name <> ".solver input) === fmap render (Primary.solver input) }))"
    | otherwise = "OpsTests.testProperty \"" <> name <> " agrees with primary\" (OpsProperty.property (do { input <- OpsProperty.forAll (pure (\"\" :: Text)); fmap OpsAnswers.render (OpsCandidate" <> name <> ".solver input) OpsProperty.=== fmap OpsAnswers.render (OpsPrimary.solver input) }))"

propertyPrefix :: Text -> Text
propertyPrefix source = bool "      " "    , " (any (`T.isInfixOf` source) ["testProperty \"", "testCase \""])
