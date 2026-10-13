{- | Offline measurement arithmetic and serialization. Counts are instructions;
durations are microseconds and RSS is KiB. Missing counters remain absent.
-}
module Ops.Measurement (Stats (..), Sample (..), stats, cpuStats, parsePerf, parsePerfCpu, perfUnavailable, parseUsage, runCount, canonical, csvHeader, csvRow, quote, cpuList) where

import Data.Text qualified as T

data Stats = Stats {median :: Rational, minimum :: Rational, maximum :: Rational}
    deriving stock (Eq, Show)

data Sample = Sample {wall :: Integer, cpu :: Maybe Integer, rss :: Integer}

-- | A missing sample invalidates the whole CPU statistic, never just that run.
cpuStats :: [Sample] -> Maybe Stats
cpuStats samples = traverse (.cpu) samples >>= stats

stats :: [Integer] -> Maybe Stats
stats values = case sort values of
    [] -> Nothing
    first' : rest -> do
        let ordered = first' : rest
            size = foldl' (\count _ -> count + 1) (0 :: Int) ordered
        lower <- viaNonEmpty head (drop ((size - 1) `div` 2) ordered)
        upper <- viaNonEmpty head (drop (size `div` 2) ordered)
        pure (Stats ((fromInteger lower + fromInteger upper) / 2) (fromInteger first') (fromInteger (foldl' max first' rest)))

parsePerf :: Text -> Either Text (Maybe Integer)
parsePerf text = case [T.strip count | line <- lines text, count : _unit : event : _ <- [T.splitOn "," line], T.strip event `elem` ["instructions:u", "cpu_core/instructions/u", "cpu_atom/instructions/u"]] of
    [] -> Left "missing perf instruction count"
    values -> do
        counts <- traverse parseCount values
        case catMaybes counts of
            [] -> Right Nothing
            [count] -> Right (Just count)
            _ambiguous -> Left "ambiguous perf instruction counts (CPU migration?)"
  where
    parseCount :: Text -> Either Text (Maybe Integer)
    parseCount value
        | value `elem` ["<not counted>", "<not supported>"] = Right Nothing
        | otherwise = case readMaybe (toString value) of
            Just count | count >= 0 -> Right (Just count)
            _invalid -> Left "invalid perf instruction count"

{- | perf's default report prints user/system seconds to nine decimal places.
Values below one microsecond are unresolved at our stored resolution.
-}
parsePerfCpu :: Text -> Either Text (Maybe Integer)
parsePerfCpu text = do
    user <- component "user"
    system <- component "sys"
    pure
        ( case (user, system) of
            (Just user', Just system') | user' + system' >= 1 -> Just (round (user' + system'))
            _unresolved -> Nothing
        )
  where
    component :: Text -> Either Text (Maybe Double)
    component name = case [value | line <- lines text, [value, "seconds", kind] <- [words line], kind == name] of
        [] -> Right Nothing
        [value] -> Just <$> seconds value
        _ambiguous -> Left "ambiguous perf CPU time"

-- | Only explicit permission/PMU failures are recoverable; other errors are fatal.
perfUnavailable :: Text -> Bool
perfUnavailable diagnostic = any (`T.isInfixOf` T.toLower diagnostic) ["no permission to enable", "permission denied", "access to performance monitoring", "operation not permitted", "pmu is not available", "cannot find pmu", "no such device", "not supported"]

parseUsage :: Text -> Either Text Integer
parseUsage text = case words text of
    [memory] -> do
        memory' <- maybeToRight "invalid max RSS" (readMaybe (toString memory))
        if memory' < 0 then Left "negative max RSS" else Right memory'
    _invalid -> Left "invalid GNU time output"

seconds :: Text -> Either Text Double
seconds value = case readMaybe @Double (toString value) of
    Just number | number >= 0 && not (isInfinite number || isNaN number) -> Right (number * 1000000)
    _invalid -> Left "invalid CPU time"

runCount :: Integer -> Int
runCount duration = if duration > 1000000 then 3 else 10

canonical :: Text -> Text -> Either Text ()
canonical branch status
    | T.strip branch /= "main" = Left "record requires branch main"
    | not (T.null (T.strip status)) = Left "record requires a clean tracked tree"
    | otherwise = Right ()

csvHeader :: Text
csvHeader = "date,commit,host,os,cpu,compiler,std,year,day,solution,runs,work_med,work_min,work_max,instr_med,wall_min_us,wall_med_us,cpu_med_us,rss_kib\n"

{- | Text cells are quoted, numeric cells round to integer schema units; Nothing
is an empty cell. Embedded quotes/newlines follow standard CSV escaping.
-}
csvRow :: [Either Text (Maybe Rational)] -> Text
csvRow cells = T.intercalate "," (map renderCell cells) <> "\n"
  where
    renderCell = either quote (maybe "" decimal)
    decimal :: Rational -> Text
    decimal value = show (round value :: Integer)

quote :: Text -> Text
quote value = "\"" <> T.replace "\"" "\"\"" value <> "\""

cpuList :: Text -> Either Text [Int]
cpuList text = fmap concat (traverse range (T.splitOn "," (T.strip text)))
  where
    range :: Text -> Either Text [Int]
    range value = case traverse (readMaybe . toString) (T.splitOn "-" value) of
        Just [single] | single >= 0 -> Right [single]
        Just [low, high] | low >= 0 && high >= low -> Right [low .. high]
        _invalid -> Left "invalid CPU list"
